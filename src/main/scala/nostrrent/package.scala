
import java.io.File
import scala.util.Random
import java.time.{ Instant, ZoneOffset }
import java.time.format.DateTimeFormatter
import java.util.Locale
import nostrrent.bittorrent.BTHash
import scala.collection.concurrent.TrieMap

package object nostrrent:

  def Option[O, I <: O | Null](i: I): Option[O] = scala.Option(i.asInstanceOf[O])

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
  def throwIAE(msg: String, cause: Throwable = null) =
    throw new IllegalArgumentException(msg, cause)

  extension[T](opt: Option[T])
    inline def ||[B >: T](orElse: => B): B = opt.getOrElse(orElse)

  val TempDir = new File(sys.props("java.io.tmpdir"))
  log.info(s"Temp dir: $TempDir")

  opaque type BTMHash = String
  extension(btmHash: BTMHash)
    def toBytes(): Array[Byte] =
      Toolbox.hexToByteArray(btmHash)
    def asBTHash: BTHash =
      BTHash(btmHash)

  object BTMHash:
    final val Regex = "[a-fA-F0-9]{64}".r
    def apply(hexHash: String): BTMHash =
      require(
        Regex.matches(hexHash),
        s"Must be 64 char hex: $hexHash")
      hexHash.toLowerCase

  /** Torrent identifier. */
  opaque type NostrrentID = String
  extension(id: NostrrentID)
    def locked[R](thunk: => R): R =
      NostrrentID.lock(id)(thunk).getOrElse:
        throw IllegalStateException(s"Torrent currently being processed")
  object NostrrentID:
    private final val LockMap = TrieMap[NostrrentID, Unit]()
    private final val RandoLen = 6
    private val TimeFormat = DateTimeFormatter.ofPattern("uuuuMMdd-HHmm-", Locale.ROOT).withZone(ZoneOffset.UTC)
    private val Regex = s"^\\d{8}-\\d{4}-[A-Z]{$RandoLen}$$".r
    def apply(): NostrrentID =
      val sb = java.lang.StringBuilder(TimeFormat.format(Instant.now))
      (1 to RandoLen).foreach(_ => sb.append { Random.between('A', 'Z'+1).toChar } )
      sb.toString
    def apply(maybeID: String): NostrrentID =
      if unapply(maybeID) then maybeID
      else throwIAE(s"Invalid identifier: $maybeID")
    def unapply(maybeID: String): Boolean =
      Regex matches maybeID
    def lock[R](id: NostrrentID)(thunk: => R): Option[R] =
      LockMap.putIfAbsent(id, ()) match
        case None =>
          try Some(thunk)
          finally LockMap.remove(id): Unit
        case Some(_) => None
