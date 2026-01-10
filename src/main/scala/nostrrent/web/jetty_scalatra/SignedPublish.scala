package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.*

import org.{ scalatra => http }
import nostrrent.nostr.NostrSignature
import org.eclipse.jetty.http.MimeTypes
import scala.util.Failure
import scala.util.Success

trait SignedPublish
extends UploadPublish:
  torrentServlet: TorrentServlet =>

  private final val UploadPath = "/signed/upload"

  // Initial upload
  post(s"$UploadPath/?"):
    uploadFiles() match
      case Left(err) =>
        http.BadRequest(err)
      case Right(id) =>
        val btHashHex = bt.generateBTHash(id, version)
        contentType = MimeType.JSON
        val json = s"""{"id":"$id","hash":"$btHashHex"}"""
        http.Accepted(json)

  // Additional upload
  post(s"$UploadPath/:$NostrrentIDParm/?"):
    val id = NostrrentID(params(NostrrentIDParm))
    id.locked:
      uploadFiles(id) match
        case Left(err) => err
        case Right(id) =>
          val btHashHex = bt.generateBTHash(id, version)
          contentType = MimeType.JSON
          val json = s"""{"id":"$id","hash":"$btHashHex"}"""
          http.Accepted(json)

  private final val PublishPath = "/signed/publish"

  put(s"$PublishPath/:$NostrrentIDParm/?"):
    val id = NostrrentID(params(NostrrentIDParm))
    withProof: nostrSig =>
      id.locked:
        publishFiles(id, version, hideServer, Some(nostrSig))

  private def withProof(thunk: NostrSignature => http.ActionResult): http.ActionResult =
    val requestType = request.contentType.map(MimeTypes.getBase) match
      case Some(contentType) => contentType
      case None =>
        if request.body.startsWith("{") then MimeType.JSON
        else MimeType.FormData

    val proof = requestType match
      case MimeType.JSON => Right:
        Proof.fromJSON(request.body)
      case MimeType.FormData => Right:
        Proof.fromFormData(params.toMap)
      case other => Left:
        http.UnsupportedMediaType(s"Cannot process: $other", Map("Connection" -> "close"))

    proof match
      case Left(failed) => failed
      case Right(Failure(ex)) => throw ex
      case Right(Success(nostrSig)) => thunk(nostrSig)
