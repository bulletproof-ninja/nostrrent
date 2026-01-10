package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.*

import org.{ scalatra => http }
import scala.util.{ Success, Failure }
import nostrrent.nostr.NostrSignature

trait UploadPublish:
  torrentServlet: TorrentServlet =>

  private final val FilenameParm = "filename"
  protected final val NostrrentIDParm = "nostrrentID"

  protected def version: bittorrent.Version =
    import bittorrent.Version
    val v1 = params.get("v1").isDefined
    val v2 = params.get("v2").isDefined
    (v1, v2) match
      case (true, false) => Version.v1
      case (false, true) => Version.v2
      case _ => Version.hybrid

  protected def hideServer: Boolean =
    params.getBoolean("hideServer", false)

  protected def uploadFiles(
    appendExisting: NostrrentID | Null = null)
    : Either[http.ActionResult, NostrrentID] =

    val isMultipartFormData = request.contentType.exists(_.startsWith(MimeType.Multipart))

    params.get(FilenameParm) match

      case Some(_) if isMultipartFormData => Left:
        http.BadRequest(s"For ${MimeType.Multipart}, remove `$FilenameParm` query parameter")

      case Some(filename) =>
        request.getContentLength match
          case -1 => Left:
            http.LengthRequired("Content-Length missing")
          case 0 => Left:
            http.BadRequest("No content")
          case _ => Right:
            val file = Iterator.single(filename -> request.getInputStream)
            bt.saveFiles(file, Option(appendExisting))

      case None if isMultipartFormData => Right:
        val files =
          fileMultiParams.iterator
            .flatMap(_._2)
            .map: item =>
              item.name -> item.part.getInputStream
        bt.saveFiles(files, Option(appendExisting))

      case None => Left:
        http.BadRequest(s"If not ${MimeType.Multipart}, must provide $FilenameParm query parameter")

  end uploadFiles

  protected def publishFiles(
    id: NostrrentID, version: bittorrent.Version, hideServer: Boolean,
    nostrSig: Option[NostrSignature] = None): http.ActionResult =

    val webSeedURL =
      serverPaths.wsPathPrefix
        .filterNot(_ => hideServer)
        .map { makeURL(_, "") }

    bt.publishTorrent(id, nostrSig, version, webSeedURL) match
      case Failure(tooBig: TorrentTooBig) =>
        http.RequestEntityTooLarge(errMsg(tooBig))
      case Failure(UnknownTorrent(id)) =>
        http.NotFound(s"Unknown torrent: $id")
      case Failure(ise: IllegalStateException) =>
        http.Conflict(errMsg(ise))
      case Failure(th: Throwable) => throw th
      case Success(seedInfo) =>
        val location =
          serverPaths.xsPathPrefix
            .map(makeURL(_, s"$id$TorrentFileExt"))
        val magnet = seedInfo.magnet.copy(dn = None)
        val magnetLink =
          location.filterNot(_ => hideServer)
            .map(xsURL => magnet.copy(xs = xsURL :: Nil))
            .getOrElse(magnet)
            .toString()
        contentType = MimeType.TorrentFile
        val headers =
          location.map(url => Map("Location" -> url.toString)).getOrElse(Map.empty)
          + ("X-Magnet-Link" -> magnetLink)
        http.Created(seedInfo.torrentBytes, headers)

  private final val UploadPath = "/upload"

  // Initial upload
  post(s"$UploadPath/?"):
    uploadFiles() match
      case Left(err) => err
      case Right(id) =>
        contentType = MimeType.JSON
        val json = s"""{"id":"$id"}"""
        http.Accepted(json)

  // Additional uploads
  post(s"$UploadPath/:$NostrrentIDParm/?"):
    val id = NostrrentID(params(NostrrentIDParm))
    id.locked:
      uploadFiles(id) match
        case Left(err) => err
        case Right(_) =>
          http.Accepted()

  private final val PublishPath = "/publish"

  put(s"$PublishPath/:$NostrrentIDParm/?"):
    val id = NostrrentID(params(NostrrentIDParm))
    id.locked:
      publishFiles(id, version, hideServer)

  // Upload and publish immediately
  post(s"$PublishPath/?"):
    uploadFiles() match
      case Left(err) => err
      case Right(id) =>
        publishFiles(id, version, hideServer)
