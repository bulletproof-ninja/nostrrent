package nostrrent.bittorrent

import java.net.URL
import nostrrent.*
import scala.util.matching.Regex
import java.net.URI

final case class MagnetLink(
  btHash: BTHash,
  btmHash: Option[BTMHash] = None,
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
    str ++= "magnet:?xt=" ++= btHash.urn
    btmHash.foreach: btmHash =>
      assert(btHash.len == 40) // If btmHash is present, then btHash must be SHA-1
      str ++= "&xt=" ++= btmHash.asBTHash.urn
    append("dn", dn)
    append("ws", ws)
    append("xs", xs)
    append("tr", tr)
    str.result()


object MagnetLink:
  import java.net.{ URLEncoder, URLDecoder }
  import java.nio.charset.StandardCharsets.UTF_8

  private val ExtractSha256Hex = s"[?&]xt=urn:btmh:1220(${BTMHash.Regex})".r
  private val ExtractSha1Hex = s"[?&]xt=urn:btih:([a-fA-F0-9]{40})".r
  private val ExtractDN = s"[?&]dn=([^&]+)".r
  private val ExtractWS = s"[?&]ws=([^&]+)".r
  private val ExtractXS = s"[?&]xs=([^&]+)".r
  private val ExtractTR = s"[?&]xs=([^&]+)".r

  extension(regex: Regex)
    def firstMatch(in: String): Option[String] = regex.findFirstMatchIn(in).map(_.group(1).toLowerCase)

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

    val sha1Hash = ExtractSha1Hex.firstMatch(magnetLink).map(BTHash(_))
    val sha256Hash = ExtractSha256Hex.firstMatch(magnetLink).map(BTMHash(_))
    val btHash = sha1Hash.orElse(sha256Hash.map(_.asBTHash)) || throwIAE(s"Invalid magnet link, no hash found: $magnetLink")
    val btmHash = sha256Hash.filter(_ != btHash)
    val dn = ExtractDN.findFirstMatchIn(magnetLink).map(_.group(1)).map(urlDecode)
    val ws = extractURLs(ExtractWS)
    val xs = extractURLs(ExtractXS)
    val tr = extractURLs(ExtractTR)
    MagnetLink(btHash, btmHash, dn = dn, ws = ws, xs = xs, tr = tr)
