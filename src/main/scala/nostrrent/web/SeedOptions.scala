package nostrrent.web

import nostrrent.nostr.NostrSignature
import scala.util.Try
import nostrrent.IAE

final case class SeedOptions(
  signature: NostrSignature,
  hideHttpServer: Boolean,
)

object SeedOptions:
  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*

  private final case class JSON(
    pubKey: String, hashSig: String,
    hideHttpServer: Boolean = false,
  )

  private given JsonValueCodec[JSON] = JsonCodecMaker make CodecMakerConfig
    .withSkipUnexpectedFields(false)

  def fromJSON(json: String): Try[SeedOptions] = Try:
    val JSON(pubKey, hashSig, hideHttpServer) =
      try readFromString[JSON](json)
      catch
        case jre: JsonReaderException =>
          val errMsg = jre.getMessage
          errMsg.indexOf(", offset:") match
            case -1 => throw jre
            case idx => throw IAE(errMsg.substring(0, idx), jre)

    SeedOptions(NostrSignature(pubKey, hashSig), hideHttpServer)

  def fromFormData(params: Map[String, String]): Try[SeedOptions] = Try:
    val nostrSig = NostrSignature(params("pubKey"), params("hashSig"))
    val hideHttpServer = params.getBoolean("hideHttpServer", false)
    SeedOptions(nostrSig, hideHttpServer)
