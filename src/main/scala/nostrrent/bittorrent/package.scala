package nostrrent

import java.net.URI

package object bittorrent:

  final val DefaultPort = 6881

  final val BTHashValidation = "^[0-9A-Fa-f]{40}$|^[0-9A-Fa-f]{64}$".r

  opaque type BTHash = String
  extension(btHash: BTHash)
    def toBytes(): Array[Byte] = Toolbox.hexToByteArray(btHash)
    def length: 40 | 64 = btHash.length match { case len: (40 | 64) => len }
    def asURN: String = btHash.length match
      case 40 => s"urn:btih:$btHash"
      case 64 => s"urn:btmh:1220$btHash"

  object BTHash:
    def apply(hex: String): BTHash =
      if unapply(hex) then hex.toLowerCase
      else throwIAE(s"Invalid hash string: $hex")
    def unapply(hex: String): Boolean =
      BTHashValidation matches hex

  val DefaultTrackers = {
    "udp://tracker.opentrackr.org:1337/announce" ::
    "udp://open.demonii.com:1337/announce" ::
    "udp://exodus.desync.com:6969/announce" ::
    "udp://open.stealth.si:80/announce" ::
    "udp://tracker.torrent.eu.org:451/announce" ::
    "http://torrent.nwps.ws:80/announce" ::
    Nil
  }.map(URI(_))
