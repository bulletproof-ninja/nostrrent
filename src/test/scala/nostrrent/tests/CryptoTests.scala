package nostrrent.tests

import org.scalatest.funsuite.AnyFunSuite
import scala.util.{ Random => rand }
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.crypto.CryptoUtil
import scodec.bits.ByteVector
import org.bitcoins.crypto.Sha256Digest
import java.io.File
import scala.util.Using, Using.Releasable
import java.io.{ FileInputStream, FileOutputStream }
import nostrrent.bittorrent.JLibTorrent
import nostrrent.*, nostr.NostrSignature



class CryptoTests
extends AnyFunSuite:

  private given Releasable[File] = _.delete()

  private val bt: Bittorrent = JLibTorrent(TempDir, 1024)

  private val keyPair = NostrKeyPair()

  test("Signature verification"):
    val torrentID = TorrentID.random()
    val torrentDir = File(TempDir, torrentID.toString)
    assert(torrentDir.mkdir() == true)
    Using.resource(File.createTempFile("nostrrent-", ".bin", torrentDir)): file =>
      println(s"Temp file: ${file.getAbsoluteFile}")
      file.deleteOnExit() // In case release fails
      Using.resource(FileOutputStream(file)): out =>
        out.write(rand.nextBytes(500))
      val id = bt.saveFiles(file.getName -> FileInputStream(file) :: Nil)
      val btmHash = bt.generateBTMHash(id, false)
      val hexHash = btmHash.toString
      val btHashBytes = ByteVector.fromHex(hexHash).get
      assert(btHashBytes.length == 32)
      val signature = keyPair.sign(btHashBytes)
      assert(signature.bytes.length == 64)
      val nostrSig = NostrSignature(keyPair.npub, signature.bytes.toHex)
      assert(nostrSig.verifySignature(btmHash) == true)
      val wrongSig =
        val array = signature.bytes.toArray
        array(4) = (~array(4)).asInstanceOf[Byte]
        nostrSig.copy(hashSigHex = ByteVector(array).toHex)
      assert(wrongSig.verifySignature(btmHash) == false)
      val wrongHexHash =
        val array = btHashBytes.toArray
        array(0) = (~array(0)).asInstanceOf[Byte]
        BTMHash(ByteVector(array).toHex)
      assert(nostrSig.verifySignature(wrongHexHash) == false)
