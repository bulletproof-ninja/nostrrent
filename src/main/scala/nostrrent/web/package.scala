package nostrrent

package web:

  object MimeType:
    final val TorrentFile = "application/x-bittorrent"
    final val JSON = "application/json"
    final val FormData = "application/x-www-form-urlencoded"

  extension(params: Map[String, String])
    def getBoolean(name: String, default: Boolean): Boolean =
      params.get(name).map(_.toLowerCase).map:
        case "false" => false
        case _ => true
      .getOrElse(default)
