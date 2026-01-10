package nostrrent.web

import nostrrent.nostr.NostrSignature
import scala.util.Try
import nostrrent.throwIAE

object Proof:
  import com.github.plokhotnyuk.jsoniter_scala.core.*
  import com.github.plokhotnyuk.jsoniter_scala.macros.*

  def fromJSON(json: String): Try[NostrSignature] = Try:
    case class JSON(pubKey: String, hashSig: String)
    given JsonValueCodec[JSON] = JsonCodecMaker make CodecMakerConfig
      .withSkipUnexpectedFields(false)
    val JSON(pubKey, hashSig) =
      try readFromString[JSON](json)
      catch
        case jre: JsonReaderException =>
          val errMsg = jre.getMessage
          errMsg.indexOf(", offset:") match
            case -1 => throw jre
            case idx => throwIAE(errMsg.substring(0, idx), jre)
    NostrSignature(pubKey, hashSig)

  def fromFormData(params: Map[String, String]): Try[NostrSignature] = Try:
    NostrSignature(params("pubKey"), params("hashSig"))
