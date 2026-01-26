package nostrrent

package web:

  given Conversion[MimeType, String]:
    def apply(mt: MimeType) = mt.toString

  class MimeType(override val toString: String)
  extends jakarta.activation.MimeType(toString: String):
    infix def matches(mt: MimeType): Boolean = mt != null && `match`(mt)
    infix def matches(mtStr: String | Null): Boolean = mtStr != null && `match`(mtStr)
    def unapply(mtStr: String): Boolean = matches(mtStr)
    def unapply(mt: MimeType): Boolean = matches(mt)

  object MimeType:
    final val TorrentFile = MimeType("application/x-bittorrent")
    final val JSON = MimeType("application/json")
    final val PlainText = MimeType("text/plain")
    final val FormData = MimeType("application/x-www-form-urlencoded")
    final val Multipart = MimeType("multipart/form-data")
