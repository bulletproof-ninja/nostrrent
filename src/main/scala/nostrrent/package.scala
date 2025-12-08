
import java.io.File
import scala.util.Random
import java.nio.ByteBuffer

package object nostrrent:

  def Logger(clz: Class[?]) =
    val name =
      clz.getName.substring(clz.getPackage.getName.length+1) match
        case "package$" => clz.getPackage.getName
        case name =>
          if name.endsWith("$") then name.dropRight(1)
          else name
    com.typesafe.scalalogging.Logger(name)

  private val log = Logger(this.getClass)

  final val TorrentFileExt = ".torrent"

  @inline
  def IAE(msg: String, cause: Throwable = null) = new IllegalArgumentException(msg, cause)

  extension[T](opt: Option[T])
    def ||[B >: T](orElse: => B): B = opt.getOrElse(orElse)

  val TempDir = new File(sys.props("java.io.tmpdir"))
  log.info(s"Temp dir: $TempDir")

  opaque type BTMHash = String
  extension(btmHash: BTMHash)
    def toBytes(): Array[Byte] =
      Toolbox.hexToByteArray(btmHash)
  object BTMHash:
    final val Regex = "[a-f0-9]{64}".r
    def apply(hexHash: String): BTMHash =
      require(
        Regex.matches(hexHash),
        s"Must be 64 char hex: $hexHash")
      hexHash

  /** 20 char identifier. */
  opaque type TorrentID = String
  object TorrentID:
    import org.ngengine.bech32.*
    private val Regex = "^[02-9ac-hj-np-z]{20}$".r
    private final val Bech32Prefix = "nostrrent"
    val random: () => TorrentID =
      val PrefixBytes = Bech32Prefix.getBytes()
      () => Bech32.bech32Encode(
          PrefixBytes,
          ByteBuffer.wrap(Random.nextBytes(12))
        )
        .drop(Bech32Prefix.length + 1)
        .dropRight(6) // Remove checksum
    def apply(unqualifiedBech32: String): TorrentID =
      require(
        Regex.matches(unqualifiedBech32),
        s"Invalid identifier: $unqualifiedBech32")
      unqualifiedBech32
