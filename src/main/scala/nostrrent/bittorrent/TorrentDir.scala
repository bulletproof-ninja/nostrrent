package nostrrent.bittorrent

import nostrrent.*
import java.io.File
import TorrentDir.*

enum TorrentDir(override val toString: String, val saveDir: File):
  def path: File

  def torrentFile: File =
    File(path.getParent, s"${path.getName}$TorrentFileExt")

  def exists(): Boolean = path.isDirectory()

  def ensurePath(): this.type =
    if ! path.exists() then
      if ! path.mkdir() then sys.error(s"Cannot create dir: $path")
    this

  def delete(): Unit =
    if torrentFile.exists() then
      throw IllegalStateException(s"Cannot delete published torrent files")
    else
      path.listFiles().foreach(_.delete())
      path.delete(): Unit

  case Nostrrent(id: NostrrentID, path: File) extends TorrentDir(id.toString, path.getParentFile)
  case External(hash: BTHash, path: File) extends TorrentDir(hash.toString, path)

object TorrentDir:

  def apply(rootTorrentDir: File, id: NostrrentID): Nostrrent =
    val path = File(rootTorrentDir, id.toString).getCanonicalFile()
    Nostrrent(id, path)

  def apply(rootTorrentDir: File, hash: BTHash): External =
    val path = File(rootTorrentDir, hash.toString).getCanonicalFile()
    External(hash, path)
