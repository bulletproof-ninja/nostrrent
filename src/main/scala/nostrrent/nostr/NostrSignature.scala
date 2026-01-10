package nostrrent.nostr

import nostrrent.*, Toolbox.hexToByteArray
import org.ngengine.bech32.Bech32
import java.nio.ByteBuffer
import nostrrent.bittorrent.BTHash

/**
  * Nostr signature.
  * @param npub Nostr public key (Bech32)
  * @param hashSigHex Nostr signature of BT hash (128 char hex)
  */
final case class NostrSignature(npub: String, hashSigHex: String):
  import NostrSignature.*

  require(npubValidation.matches(npub), s"Invalid npub: $npub")
  require(sigValidation.matches(hashSigHex), s"Hash signature must be 128 character hexadecimal encoding")

  def hashSigBytes = hexToByteArray(hashSigHex)

  def verifySignature(btHash: BTHash): Boolean =
    nostrrent.nostr.verifySignature(btHash, this)

object NostrSignature:
  private val npubValidation = """^npub1[02-9ac-hj-np-z]{58}$""".r
  private val sigValidation = """^[0-9a-fA-F]{128}$""".r

  private val hexToNpub: String => String =
    val prefix = "npub".getBytes
    hexKey =>
      val pubKeyBytes = hexToByteArray(hexKey)
      Bech32.bech32Encode(prefix, ByteBuffer.wrap(pubKeyBytes))

  /**
    * New instance.
    * @param pubKey Nostr public key, either Bech32 or hexadecimal
    * @param hashSigHex Nostr signature of BT hash (128 char hex)
    */
  def apply(pubKey: String, hashSigHex: String): NostrSignature =
    val presumedNpub = pubKey.length match
      case 63 => pubKey
      case 64 => hexToNpub(pubKey)
      case _ => throwIAE(s"Invalid public key: $pubKey")
    new NostrSignature(presumedNpub, hashSigHex)
