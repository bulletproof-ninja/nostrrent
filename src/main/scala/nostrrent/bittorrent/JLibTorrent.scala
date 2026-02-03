package nostrrent.bittorrent

import nostrrent.*, nostr.NostrSignature
import com.frostwire.jlibtorrent.{
  TorrentHandle, AlertListener, Sha256Hash,
  SessionManager, SessionParams, SettingsPack,
  TorrentBuilder, TorrentInfo,
  swig, alerts,
}
import alerts.{ Alert, AlertType, MetadataReceivedAlert }
import swig.{ create_torrent, create_flags_t }

import java.io.{ File, FileInputStream, ByteArrayInputStream }
import java.net.URL
import scala.util.{ Try, Using }
import scala.util.control.NonFatal
import com.frostwire.jlibtorrent.Sha1Hash

class JLibTorrent private(rootTorrentDir: File, ioBufferSize: Int)
extends nostrrent.LocalFileSystem(rootTorrentDir, ioBufferSize)
with AutoCloseable:
  import JLibTorrent.*

  private val session = SessionManager() // Configure and start in companion object

  private val CreateFlags = Map(
    Version.v1 -> { create_flags_t() or_ create_torrent.v1_only },
    Version.v2 -> { create_flags_t() or_ create_torrent.v2_only },
    Version.hybrid -> create_flags_t(),
  )

  private def TorrentBuilder(path: File, version: bittorrent.Version) =
    new TorrentBuilder().flags(CreateFlags(version)).path(path)

  def close(): Unit = session.stop()

  def generateBTHash(id: NostrrentID, version: bittorrent.Version): BTHash =
    val path = File(workDir, id.toString)
    val torrent = TorrentBuilder(path, version).generate()
    val hash = TorrentInfo(torrent.entry.bencode)
    hash.asBTHash

  protected def publishedMagnet(id: NostrrentID): Option[MagnetLink] =
    val torrentDir = TorrentDir(rootTorrentDir, id)
    val torrentFile = torrentDir.torrentFile
    if torrentFile.isFile then Some:
      val magnet = loadTorrent(torrentFile).info.makeMagnetUri()
      MagnetLink.parse(magnet)
    else None

  def publishTorrent(
    id: NostrrentID, sig: Option[NostrSignature],
    version: bittorrent.Version, webSeedURL: Option[URL]) =
    Try:
      val torrentDir = TorrentDir(rootTorrentDir, id)
      if ! torrentDir.exists() then throw UnknownTorrent(id)
      val torrentFile = torrentDir.torrentFile
      val proof = sig.map(Proof.toComment)

      val (info, torrentBytes) =
        if torrentFile.exists() then
          loadTorrent(torrentFile)
        else
          val tb = TorrentBuilder(torrentDir.path, version).creator(Creator)
          proof.foreach(tb.comment)
          webSeedURL.map(_.toString).foreach(tb.addUrlSeed)
          val torrent = tb.generate()
          val torrentBytes = torrent.entry.bencode
          TorrentInfo.bdecode(torrentBytes) -> torrentBytes

      val btHash = info.asBTHash

      if torrentBytes.length > MaxTorrentFileSize then
        throw TorrentTooBig(torrentBytes.length)
      if proof.exists(_ != info.comment) then
        throw IllegalStateException("Signature mismatch")
      if sig.exists(sig => ! sig.verifySignature(btHash)) then
        throwIAE("Signature validation failed")

      if ! torrentFile.exists() then
        this.writeNewFile(
          ByteArrayInputStream(info.bencode),
          torrentFile)

      JLibTorrent.seed(session, torrentDir, info)
      val magnet = MagnetLink.parse(info.makeMagnetUri())
      magnet -> torrentBytes

  def seedTorrent(torrentBytes: Array[Byte]): Option[Float] =
    val info = try TorrentInfo.bdecode(torrentBytes) catch
      case NonFatal(th) => throwIAE("Invalid torrent", th)
    if ! info.isValid() then throwIAE("Invalid torrent")
    session.find(info) match
      case handle: TorrentHandle => Some:
        handle.status.progress()
      case null =>
        val btHash = info.asBTHash
        val proof = Proof.fromComment(info.comment)
        if proof.exists(sig => ! sig.verifySignature(btHash)) then
          throwIAE("Signature verification failed")
        val torrentDir = TorrentDir(rootTorrentDir, btHash).ensurePath()
        writeNewFile(
          ByteArrayInputStream(info.bencode),
          torrentDir.torrentFile)
        JLibTorrent.seed(session, torrentDir, info); None

  def seedTorrent(magnet: MagnetLink): Option[Float] =
    val torrent: (handle: TorrentHandle, hash: (Sha1Hash | Sha256Hash)) =
      magnet.btHash.length match
        case 40 =>
          val hash = Sha1Hash(magnet.btHash.toString)
          session.find(hash) -> hash
        case 64 =>
          val hash = Sha256Hash(magnet.btHash.toString)
          (session.find(hash) -> hash)

    torrent.handle match
      case handle: TorrentHandle => Some:
        handle.status.progress()
      case null =>
        val torrentDir = TorrentDir(rootTorrentDir, magnet.btHash).ensurePath()
        session.addListener:
          new AlertListener:
            def types() = new Array(AlertType.METADATA_RECEIVED.swig)
            def alert(x: Alert[?]) = x match
              case alert: MetadataReceivedAlert =>
                val info = alert.handle.torrentFile
                val isMatch = torrent.hash match
                  case sha1: Sha1Hash => sha1 == info.infoHashV1
                  case sha256: Sha256Hash => sha256 == info.infoHashV2
                if isMatch then
                  session.removeListener(this)
                  writeNewFile(
                    ByteArrayInputStream(info.bencode),
                    torrentDir.torrentFile)
        JLibTorrent.seed(session, torrentDir, magnet); None

end JLibTorrent

object JLibTorrent:
  import com.frostwire.jlibtorrent.FileStorage.*

  private val log = Logger(this.getClass)

  final val Creator = "Nostrrent"

  extension[T](fileFlags: swig.file_flags_t)
    def notPadding: Boolean = ! fileFlags.and_(FLAG_PAD_FILE).nonZero()

  extension(info: TorrentInfo)
    def asBTHash: BTHash = BTHash:
      Option(info.infoHashV2).map(_.toHex) || info.infoHashV1.toHex

  private def deepDelete(file: File): Unit = if file.exists then
    Option(file.listFiles).foreach(_.foreach(deepDelete))
    if ! file.delete() then log.warn(s"Could not delete: $file")

  def apply(rootTorrentDir: File, ioBufferSize: Int): JLibTorrent =
    val impl = new JLibTorrent(rootTorrentDir, ioBufferSize)
    val seedFolders =
      rootTorrentDir.listFiles(_.isDirectory)
        .flatMap: folder =>
          val torrentDir = folder.getName match
            case id @ NostrrentID() => Some:
              TorrentDir.Nostrrent(NostrrentID(id), folder)
            case hash @ BTHash() => Some:
              TorrentDir.External(BTHash(hash), folder)
            case _ => None
          torrentDir.flatMap: torrentDir =>
            val torrentFile = torrentDir.torrentFile
            if torrentFile.isFile then
              Some(torrentDir)
            else // Cleanup:
              deepDelete(torrentFile) // Not file, so either dir (unlikely) or non-existant
              deepDelete(folder)
              None
        .toList
    startSession(impl.session, seedFolders)
    impl

  import com.frostwire.jlibtorrent.{ TorrentFlags => tf }
  private final val CompleteTorrentFlags =
    tf.SEED_MODE or_
    tf.UPLOAD_MODE
  private final val UnknownStatusFlags =
    tf.SEQUENTIAL_DOWNLOAD

  private def seed(session: SessionManager, torrentDir: TorrentDir, info: TorrentInfo): Unit =
    val infoHash = info.infoHashV2() match
      case null => info.infoHashV1().toHex()
      case v2 => v2.toHex()
    log.info(s"Seeding $infoHash: `${torrentDir.path}` from `${torrentDir.saveDir}`")
    val seedFlags = torrentDir match
      case _: TorrentDir.Nostrrent => CompleteTorrentFlags
      case _ => UnknownStatusFlags
    session.download(info, torrentDir.saveDir, null, null, null, seedFlags)

  private def seed(session: SessionManager, torrentDir: TorrentDir.External, magnet: MagnetLink): Unit =
    session.download(magnet.toString, torrentDir.saveDir, UnknownStatusFlags)

  private def loadTorrent(torrentFile: File): (info: TorrentInfo, torrentBytes: Array[Byte]) =
    val bytes = Using.resource(FileInputStream(torrentFile)) { _.readAllBytes() }
    TorrentInfo.bdecode(bytes) -> bytes

  private def startSession(session: SessionManager, existing: IterableOnce[TorrentDir]): Unit =
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
            .toSet
            .filterNot: at => // Remove noise:
              at == AlertType.UNKNOWN ||
              at == AlertType.SESSION_STATS ||
              at == AlertType.SESSION_STATS_HEADER ||
              at == AlertType.STATE_UPDATE ||
              at == AlertType.INCOMING_CONNECTION ||
              at.name.startsWith("DHT_") ||
              at.name.startsWith("PEER_") ||
              at.name.startsWith("LISTEN_") ||
              at.name.startsWith("FASTRESUME_")
            .concat: // Ensure specific alerts:
              Set(
                AlertType.DHT_STATS,
                AlertType.PEER_BLOCKED,
                AlertType.DHT_IMMUTABLE_ITEM,
                AlertType.DHT_MUTABLE_ITEM,
              )
            .map(_.swig)
            .toArray

    session.start(SessionParams(settings))

    // Start seeding existing torrents:
    existing.iterator.foreach:
      case torrentDir =>
        val info = loadTorrent(torrentDir.torrentFile).info
        seed(session, torrentDir, info)

  end startSession
