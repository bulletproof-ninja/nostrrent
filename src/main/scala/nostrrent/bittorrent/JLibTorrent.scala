package nostrrent.bittorrent

import nostrrent.*, nostr.NostrSignature
import com.frostwire.jlibtorrent.{
  SessionManager, SessionParams, SettingsPack,
  TorrentBuilder, TorrentInfo,
  swig
}
import java.io.{ File, FileInputStream, ByteArrayInputStream }
import scala.util.Try
import java.net.URL
import com.frostwire.jlibtorrent.AlertListener
import com.frostwire.jlibtorrent.alerts.{ Alert, AlertType }
import com.frostwire.jlibtorrent.swig.{ create_torrent, create_flags_t }

class JLibTorrent(rootTorrentDir: File, ioBufferSize: Int)
extends nostrrent.LocalFileSystem(rootTorrentDir, ioBufferSize)
with AutoCloseable:

  import JLibTorrent.*

  private val session = SessionManager() // Configure and start in companion object

  private val CreateHybrid = create_flags_t()
  private val CreateV2Only = create_flags_t() or_ create_torrent.v2_only
  private def TorrentBuilder(saveDir: File, hybrid: Boolean) =
    val flags = if hybrid then CreateHybrid else CreateV2Only
    new TorrentBuilder().flags(flags).path(saveDir)

  def close(): Unit = session.stop()

  def generateBTMHash(torrentID: TorrentID, hybrid: Boolean): BTMHash =
    val saveDir = File(workDir, torrentID.toString)
    val torrent = TorrentBuilder(saveDir, hybrid).generate()
    val hash = TorrentInfo(torrent.entry.bencode).infoHashV2().toHex()
    BTMHash(hash)


  def verifyAndSeed(
    torrentID: TorrentID, sig: NostrSignature,
    hybrid: Boolean, webSeedURL: Option[URL]): Try[SeedInfo] =
    Try:

      val saveDir = File(workDir, torrentID.toString).getCanonicalFile()
      if ! saveDir.isDirectory then throw UnknownTorrent(torrentID)

      val torrentFile = File(rootTorrentDir, s"$torrentID$TorrentFileExt")
      val proof = s"${sig.npub}:${sig.hashSigHex}"

      val (info, torrentBytes) =
        if torrentFile.exists() then
          loadTorrent(torrentFile)
        else
          val torrent =
            TorrentBuilder(saveDir, hybrid)
              .creator(Creator)
              .comment(proof)
              .generate()
          val torrentBytes = torrent.entry.bencode
          TorrentInfo.bdecode(torrentBytes) -> torrentBytes

      if torrentBytes.length > MaxTorrentFileSize then
        throw TorrentTooBig(torrentBytes.length)
      if info.comment != proof then
        throw IllegalStateException("Signature mismatch")
      if ! sig.verifySignature(info.btmHash()) then
        throw IAE("Signature validation failed")

      webSeedURL.foreach: webSeedURL =>
        info.addUrlSeed(webSeedURL.toString)

      if ! torrentFile.exists() then
        this.writeNewFile(
          ByteArrayInputStream(info.bencode),
          torrentFile)

      // if session.find(info) == null then // Race condition, but no obvious way to avoid
      JLibTorrent.seed(session, saveDir, info)

      val magnet = MagnetLink.parse(info.makeMagnetUri())
      SeedInfo(magnet, torrentBytes)

end JLibTorrent

object JLibTorrent:
  import com.frostwire.jlibtorrent.FileStorage.*

  private val log = Logger(this.getClass)

  final val Creator = "Nostrrent"
  final val MaxTorrentFileSize = 10*1024*1024

  extension[T](fileFlags: swig.file_flags_t)
    def notPadding: Boolean = ! fileFlags.and_(FLAG_PAD_FILE).nonZero()

  extension (info: TorrentInfo)
    def btmHash(): BTMHash =
      BTMHash(info.infoHashV2().toHex)

  private case class SeedFile(torrentFile: File, torrentFolder: File):
    assert(torrentFile.isFile)
    assert(torrentFolder.isDirectory)
    assert(torrentFile.getName.startsWith(torrentFolder.getName))
    assert(torrentFile.getName.endsWith(TorrentFileExt))

  private def deleteDir(dir: File): Unit =
    Option(dir.listFiles()).foreach(_.foreach(_.delete()))
    dir.delete()

  def apply(rootTorrentDir: File, ioBufferSize: Int): JLibTorrent =
    val impl = new JLibTorrent(rootTorrentDir, ioBufferSize)
    val seedFolders =
      rootTorrentDir.listFiles(_.isDirectory)
        .flatMap: folder =>
          val torrentFile = File(rootTorrentDir, s"${folder.getName}$TorrentFileExt")
          if torrentFile.isFile then
            Some(SeedFile(torrentFile, folder))
          else // Cleanup:
            deleteDir(torrentFile)
            deleteDir(folder)
            None
        .toList
    startSession(impl.session, seedFolders)
    impl

  import com.frostwire.jlibtorrent.{ TorrentFlags => tf }
  private final val SeedFlags =
    tf.SEED_MODE or_
    tf.SHARE_MODE or_
    tf.UPLOAD_MODE

  private def seed(session: SessionManager, torrentDir: File, info: TorrentInfo): Unit =
    log.info(s"Seeding `${torrentDir}` | ${info.infoHashV2.toHex}")
    session.download(info, torrentDir, null, null, null, SeedFlags)

  private def loadTorrent(torrentFile: File): (info: TorrentInfo, torrentBytes: Array[Byte]) =
    val bytes = FileInputStream(torrentFile).readAllBytes()
    TorrentInfo.bdecode(bytes) -> bytes

  private def startSession(session: SessionManager, existing: IterableOnce[SeedFile]): Unit =
    import swig.settings_pack.bool_types.*

    val settings = SettingsPack()

    Map( // Boolean settings:
      enable_upnp -> true,
      enable_natpmp -> true,
      enable_incoming_tcp -> true,
      enable_incoming_utp -> true,
      no_recheck_incomplete_resume -> true,
    ).foreach: (name, value) =>
      settings.setBoolean(name.swigValue, value)

    settings.activeDownloads(0)
    settings.activeSeeds(Int.MaxValue)
    settings.setEnableDht(true)
    settings.setEnableLsd(false)
    settings.seedingOutgoingConnections(true)

    session.addListener:
      new AlertListener:
        def alert(alert: Alert[?]): Unit = log.info(s"libtorrent: $alert")
        def types(): Array[Int] =
          AlertType.values()
            .filterNot: at => // Remove noise:
              at == AlertType.UNKNOWN ||
              at == AlertType.SESSION_STATS ||
              at == AlertType.SESSION_STATS_HEADER ||
              at == AlertType.STATE_UPDATE ||
              at.name.startsWith("DHT_") ||
              at.name.startsWith("PEER_") ||
              at.name.startsWith("LISTEN_") ||
              at.name.startsWith("FASTRESUME_")
            .map(_.swig)

    session.start(SessionParams(settings))

    // Start seeding existing torrents:
    existing.iterator.foreach:
      case SeedFile(torrentFile, folder) =>
        val (info, _) = loadTorrent(torrentFile)
        seed(session, folder, info)

  end startSession
