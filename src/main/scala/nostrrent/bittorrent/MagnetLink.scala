package nostrrent.bittorrent

import java.net.URL
import nostrrent.IAE
import scala.util.matching.Regex
import java.net.URI
import nostrrent.BTMHash

final case class MagnetLink(
  /** SHA-256 hex hash. */
  btmHash: BTMHash,
  btiHash: Option[String],
  dn: Option[String] = None,
  ws: List[URL] = Nil,
  xs: List[URL] = Nil,
  tr: List[URL] = Nil):

  import MagnetLink.urlEncode

  override def toString(): String =
    val str = StringBuilder()
    def append(parmName: String, rawValues: Iterable[Any]): Unit =
      rawValues.foreach: rawValue =>
        str += '&' ++= parmName += '='
        str ++= urlEncode(rawValue.toString)
    str ++= "magnet:?xt=urn:btmh:1220" ++= btmHash.toString
    btiHash.foreach: btiHash =>
      str += '&' ++= "xt=urn:btih:" ++= btiHash
    append("dn", dn)
    append("ws", ws)
    append("xs", xs)
    append("tr", tr)
    str.result()


object MagnetLink:
  import java.net.{ URLEncoder, URLDecoder }
  import java.nio.charset.StandardCharsets.UTF_8

  private val ExtractSha1Hex = s"[?&]xt=urn:btih:([a-f0-9]{40})".r
  private val ExtractSha256Hex = s"[?&]xt=urn:btmh:1220(${BTMHash.Regex})".r
  private val ExtractDN = s"[?&]dn=([^&]+)".r
  private val ExtractWS = s"[?&]ws=([^&]+)".r
  private val ExtractXS = s"[?&]xs=([^&]+)".r
  private val ExtractTR = s"[?&]xs=([^&]+)".r

  private def urlEncode(raw: String): String =
    URLEncoder.encode(raw, UTF_8)
  private def urlDecode(encoded: String): String =
    URLDecoder.decode(encoded, UTF_8)

  def parse(magnetLink: String): MagnetLink =

    def extractURLs(regex: Regex): List[URL] =
        regex.findAllMatchIn(magnetLink)
          .map(_.group(1))
          .map(urlDecode)
          .map(URI(_).toURL)
          .toList

    val btmHash = BTMHash:
      ExtractSha256Hex.findFirstMatchIn(magnetLink)
        .map(_.group(1))
        .getOrElse(throw IAE(s"Invalid v2 magnet link: $magnetLink"))
    val btiHash =
      ExtractSha1Hex.findFirstMatchIn(magnetLink)
        .map(_.group(1))
    val dn = ExtractDN.findFirstMatchIn(magnetLink).map(_.group(1)).map(urlDecode)
    val ws = extractURLs(ExtractWS)
    val xs = extractURLs(ExtractXS)
    val tr = extractURLs(ExtractTR)
    MagnetLink(btmHash, btiHash = btiHash, dn = dn, ws = ws, xs = xs, tr = tr)
