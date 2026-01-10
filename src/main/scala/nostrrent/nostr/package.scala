package nostrrent

import fr.acinq.secp256k1.{ Secp256k1, Secp256k1Exception }
import org.ngengine.bech32.{ Bech32, Bech32Exception }
import nostrrent.*
import nostrrent.bittorrent.*

package object nostr:

  private val secp = Secp256k1.get() // Singleton instance

  private def decodeNpub(npub: String): Array[Byte] =
    val bb = Bech32.bech32Decode(npub)
    val array = new Array[Byte](bb.capacity)
    bb.get(array)
    array

  protected[nostr] def verifySignature(btHash: BTHash, nostrSig: NostrSignature): Boolean =
    try
      val npubBytes = decodeNpub(nostrSig.npub)
      val hashBytes = btHash.toBytes()
      val sigBytes = nostrSig.hashSigBytes
      secp.verifySchnorr(sigBytes, hashBytes, npubBytes)
    catch
      case th: (Secp256k1Exception | Bech32Exception) =>
        throwIAE(th.getMessage, th)
