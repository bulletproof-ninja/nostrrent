package nostrrent

final case class UnknownTorrent(id: NostrrentID)
extends RuntimeException(s"Unknown torrent: $id")
