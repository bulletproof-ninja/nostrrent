package nostrrent

import java.io.{ File, InputStream, FileOutputStream }
import scala.annotation.tailrec
import nostrrent.nostr.NostrSignature
import scala.util.{ Try, Success, Failure }
import java.net.URL
import nostrrent.bittorrent.MagnetLink
import nostrrent.bittorrent.BTHash
import nostrrent.bittorrent.TorrentDir

object Bittorrent:
  final val MaxTorrentFileSize = 2*1024*1024

trait Bittorrent:
  final val MaxTorrentFileSize = Bittorrent.MaxTorrentFileSize

  type UploadFile = (filename: String, content: InputStream)

  /**
    * Save files to new torrent.
    * @param files
    * @param appendExisting Append existing torrent. Must be unpublished.
    * @return the new torrent identifier
    */
  def saveFiles(files: Iterator[UploadFile], appendExisting: Option[NostrrentID]): NostrrentID

  /**
    * Generate the Bittorrent multi-hash (v2).
    * @param id The torrent identifier
    * @param version Bittorrent version
    * @return Hexadecimal BT hash
    */
  def generateBTHash(id: NostrrentID, version: bittorrent.Version): BTHash

  /**
    * Publish new torrent, and verify signature if available.
    * @param id identifier
    * @param sig Optional signature
    * @param version Torrent version
    * @param webSeedURL Full URL to root of file serving
    * @return Magnet link and torrent info bytes
    */
  def publishTorrent(
    id: NostrrentID,
    sig: Option[NostrSignature],
    version: bittorrent.Version,
    webSeedURL: Option[URL]): Try[(magnet: MagnetLink, torrentBytes: Array[Byte])]

  /**
    * Seed existing torrent.
    * @param torrentBytes The torrent file
    * @return Progress, if already seeding
    */
  def seedTorrent(torrentBytes: Array[Byte]): Option[Float]

  /**
    * Seed existing torrent.
    * @param btHash The torrent SHA-1 or SHA-256 hash
    * @return Progress, if already seeding
    */
  def seedTorrent(btHash: BTHash): Option[Float]


trait LocalFileSystem(val workDir: File, ioBufferSize: Int)
extends Bittorrent:

  require(workDir == workDir.getCanonicalFile, s"Working directory must be canonical")

  if ! workDir.exists() then
    if ! workDir.mkdir() then throw SystemException(s"Cannot create dir: $workDir")

  private val WindowsIllegalChars = """[\/:\*\?"|<>]""".r
  private final val WindowsLengthLimit = 260

  def saveFiles(files: Iterator[UploadFile], appendExisting: Option[NostrrentID]): NostrrentID =
    val id = appendExisting || NostrrentID()
    val torrentDir = TorrentDir(workDir, id)

    def saveFiles(files: Iterator[UploadFile], offset: Int): NostrrentID =
      val filesWritten = Try:
        files.iterator
          .zipWithIndex.map: // Prefix files with index to preserve order
            case ((filename, inp), idx) =>
              (filename, inp, idx + offset)
          .map:
            case (filename, inp, idx) =>
              if idx > 999 then throwIAE("Too many files")
              f"$idx%03d-$filename" -> inp
          .map(writeTorrentContent(torrentDir.path))
          .size + offset
      filesWritten match
        case Failure(ex) =>
          torrentDir.path.listFiles().foreach(_.delete())
          torrentDir.path.delete()
          throw ex
        case Success(0) =>
          torrentDir.path.delete()
          throwIAE("No file(s) provided")
        case Success(_) =>
          id
    end saveFiles

    val offset =
      if appendExisting.isEmpty then // New ID:
        torrentDir.ensurePath()
        0
      else if ! torrentDir.exists() then // Invalid ID:
        throwIAE(s"Unknown id: ${torrentDir.id}")
      else if torrentDir.torrentFile.exists() then // Already created:
        throw IllegalStateException(s"Torrent already created: $id")
      else
        torrentDir.path.list()
          .flatMap(n => Try(n.take(3).toInt).toOption)
          .max + 1
    saveFiles(files, offset)

  private def writeTorrentContent(
    torrentFolder: File, ioBuffer: Array[Byte] = new Array(ioBufferSize))(
      filename: String, inp: InputStream): Unit =
    val safeLength = WindowsLengthLimit - torrentFolder.getName.length - 1
    val safeFilename = WindowsIllegalChars.replaceAllIn(filename, "_").take(safeLength)
    val file = File(torrentFolder, safeFilename)
    writeNewFile(inp, file, ioBuffer)

  def writeNewFile(inp: InputStream, file: File, ioBuffer: Array[Byte] = new Array(ioBufferSize)): Unit =
    if ! file.createNewFile() then
      if file.exists() then throw DuplicateFilename(file.getName)
      else throw SystemException(s"Cannot create file: ${file.getName}")
    val out = FileOutputStream(file)

    @tailrec
    def writeNextChunk(): Unit =
      inp.read(ioBuffer) match
        case -1 =>
          out.close()
        case bytesRead =>
          out.write(ioBuffer, 0, bytesRead)
          writeNextChunk()

    try
      writeNextChunk()
    catch case e: Exception =>
      file.delete()
      throw e
