package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.*

import org.{ scalatra => http }
import Bittorrent.*
import scala.util.Using
import nostrrent.bittorrent.BTHash
import nostrrent.bittorrent.MagnetLink

trait Seed:
  torrentServlet: TorrentServlet =>

  private def toPercent(progress: Float): String = s"${(progress * 100).toInt}%"

  put("/seed/?"):
    request.getContentLength -> request.getContentType match
      case (_, null | "") => http.NotAcceptable("Content-Type missing")
      case (-1, _) => http.LengthRequired("Content-Length missing")
      case (0, _) => http.BadRequest("No content")
      case (len, MimeType.TorrentFile()) if len > MaxTorrentFileSize => http.RequestEntityTooLarge("Torrent file too large")
      case (len, MimeType.TorrentFile()) =>
        val torrentBytes = Using.resource(request.getInputStream)(_.readNBytes(len))
        bt.seedTorrent(torrentBytes) match
          case None => http.Created()
          case Some(progress) => http.Ok(toPercent(progress))
      case (len, MimeType.PlainText()) if len > MaxMagnetLinkSize => http.RequestEntityTooLarge("Magnet link too large")
      case (len, MimeType.PlainText()) =>
        val magnet = MagnetLink.parse(request.body.take(len))
        bt.seedTorrent(magnet) match
          case None => http.Accepted()
          case Some(progress) => http.Ok(toPercent(progress))

      case (_, unacceptable) =>
        http.NotAcceptable(s"Unknown content: $unacceptable, expected: ${MimeType.TorrentFile}")

  put("/seed/:btHash"):
    val btHash = BTHash(params("btHash"))
    bt.seedTorrent(btHash) match
      case None => http.Accepted()
      case Some(progress) => http.Ok(toPercent(progress))
