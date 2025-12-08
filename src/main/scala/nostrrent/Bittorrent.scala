package nostrrent

import java.io.{ File, InputStream, FileOutputStream }
import scala.annotation.tailrec
import nostrrent.nostr.NostrSignature
import scala.util.{ Try, Success, Failure }
import java.net.URL
import nostrrent.bittorrent.MagnetLink

trait Bittorrent:

  /**
    * Save files to new torrent.
    * @param files
    * @return the new torrent identifier
    */
  def saveFiles(files: IterableOnce[(String, InputStream)]): TorrentID

  /**
    * Generate the Bittorrent multi-hash (v2).
    * @param torrentID The torrent identifier
    * @param hybrid Hybrid torrent (v1 + v2)
    * @return Hexadecimal BT hash
    */
  def generateBTMHash(torrentID: TorrentID, hybrid: Boolean): BTMHash

  case class SeedInfo(magnet: MagnetLink, torrentBytes: Array[Byte])

  /**
    * Verify signature and seed collection.
    * @param torrentID identifier
    * @param sig Signature
    * @param hybrid Create hybrid torrent (v1 + v2)
    * @param webSeedURL Full URL to root of file serving
    * @return Magnet link and torrent info bytes
    */
  def verifyAndSeed(
    torrentID: TorrentID,
    sig: NostrSignature,
    hybrid: Boolean,
    webSeedURL: Option[URL]): Try[SeedInfo]


trait LocalFileSystem(val workDir: File, ioBufferSize: Int)
extends Bittorrent:

  require(workDir == workDir.getCanonicalFile, s"Working directory must be canonical")

  if ! workDir.exists() then
    if ! workDir.mkdir() then throw SystemException(s"Cannot create dir: $workDir")

  private val WindowsIllegalChars = """[\/:\*\?"|<>]""".r

  def saveFiles(files: IterableOnce[(String, InputStream)]): TorrentID =
    @tailrec
    def saveFiles(torrentID: TorrentID, files: IterableOnce[(String, InputStream)]): TorrentID =
      val newTorrentFolder = File(workDir, torrentID.toString)
      if newTorrentFolder.exists then
        saveFiles(TorrentID.random(), files) // Torrent ID clash, recursively try again (very unlikely)
      else if ! newTorrentFolder.mkdir() then
        throw SystemException(s"Cannot create dir: $newTorrentFolder")
      else
        val filesWritten = Try:
          files.iterator
            .zipWithIndex.map: // Prefix files with index to preserve order
              case ((filename, inp), idx) =>
                if idx > 999 then throw IAE("Too many files")
                f"$idx%03d $filename" -> inp
            .map(writeTorrentContent(newTorrentFolder))
            .size
        filesWritten match
          case Failure(ex) =>
            newTorrentFolder.listFiles().foreach(_.delete())
            newTorrentFolder.delete()
            throw ex
          case Success(0) =>
            newTorrentFolder.delete()
            throw IAE("No file(s) provided")
          case Success(_) =>
            torrentID

    saveFiles(TorrentID.random(), files)

  private def writeTorrentContent(
    torrentFolder: File, ioBuffer: Array[Byte] = new Array(ioBufferSize))(
      filename: String, inp: InputStream): Unit =
    val safeFilename = WindowsIllegalChars.replaceAllIn(filename, "_")
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
