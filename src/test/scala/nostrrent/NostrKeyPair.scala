package nostrrent

import org.ngengine.bech32.Bech32
import org.bitcoins.crypto.ECPrivateKey
import java.nio.ByteBuffer
import scodec.bits.ByteVector
import org.bitcoins.crypto.SchnorrDigitalSignature
import fr.acinq.secp256k1.Secp256k1
import java.util.Arrays

object NostrKeyPair:

  def apply(privKey: ECPrivateKey = ECPrivateKey()): NostrKeyPair =
    new NostrKeyPair(privKey)

  def apply(nsecKey: String): NostrKeyPair =
    val privKeyBB = Bech32.bech32Decode(nsecKey)
    val privKey = ECPrivateKey(ByteVector(privKeyBB))
    new NostrKeyPair(privKey)

class NostrKeyPair(privKey: ECPrivateKey):

  private val secp256k1 = Secp256k1.get

  private val pubkey = privKey.publicKey.compressed.bytes.drop(1).toArray

  val npub = Bech32.bech32Encode(
    "npub".getBytes,
    ByteBuffer.wrap(pubkey),
  )

  if Arrays.equals(pubkey, decodeBech32(npub)) then println("Bech32 decode works!")
  else sys.error("Bech32 decode problem")


  def sign(bytes: Array[Byte]): SchnorrDigitalSignature =
    sign(bytes, ByteVector(bytes))
  def sign(bytes: ByteVector): SchnorrDigitalSignature =
    sign(bytes.toArray, bytes)

  private def sign(dataArray: Array[Byte], dataBytes: ByteVector) =
    assert(Arrays.equals(dataBytes.toArray, dataArray))
    val signature = privKey.schnorrSign(dataBytes)
    if secp256k1.verifySchnorr(signature.bytes.toArray, dataArray, pubkey) then println("Signature validated!")
    else sys.error("Signature validation failed!")
    signature

  private def decodeBech32(bech32: String): Array[Byte] =
    val bb = Bech32.bech32Decode(bech32)
    val array = new Array[Byte](bb.capacity)
    bb.get(array)
    array
