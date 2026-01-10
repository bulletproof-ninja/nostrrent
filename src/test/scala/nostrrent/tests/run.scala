package nostrrent.tests

import scodec.bits.ByteVector
import nostrrent.nostr.NostrSignature
import nostrrent.{ BTMHash, NostrKeyPair }

@main def signHash(hexHash: String, nsec: String*): Unit =
  require(BTMHash.Regex.matches(hexHash), s"Invalid BTM hex hash: $hexHash")
  val keys = nsec match
    case Seq() => NostrKeyPair()
    case Seq(nsec) => NostrKeyPair(nsec)
  val btHashBytes = ByteVector.fromHex(hexHash).get
  val signature = keys.sign(btHashBytes)
  assert(signature.bytes.length == 64)
  val nostrSig = NostrSignature(keys.npub, signature.bytes.toHex)
  println(s"Nostr signature: $nostrSig")
