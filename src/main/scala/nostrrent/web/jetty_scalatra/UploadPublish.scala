package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.{*, given}

import org.{ scalatra => http }
import scala.util.{ Success, Failure }
import nostrrent.nostr.NostrSignature

import scala.jdk.CollectionConverters.given
import nostrrent.bittorrent.BTHash
import scala.annotation.nowarn

trait UploadPublish:
  torrentServlet: TorrentServlet =>

  private final val FilenameParm = "filename"
  protected final val NostrrentIDParm = "nostrrentID"

  extension[AR <: http.ActionResult](result: AR)
    def withBody(id: NostrrentID, hash: BTHash | Null = null): AR =
      val (ct, body) =
        request.getHeaders("Accept").asScala
          .flatten(_.split(", ?").iterator: @nowarn) // TODO: Remove @nowarn when Scala code updated
          .map(MimeType(_))
          .++(Iterator.single(MimeType.JSON)) // Default return type
          .collectFirst:
            case ct @ MimeType.JSON() => ct -> {
              if hash == null then s"""{"id":"$id"}"""
              else s"""{"id":"$id","hash":"$hash"}"""
            }
            case ct @ MimeType.PlainText() => ct -> {
              if hash == null then id.toString
              else s"$id:$hash"
            }
          .get
      result.copy(
        body = body,
        headers = result.headers.updated("Content-Type", ct),
      ).asInstanceOf[AR]

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

    val isMultipartFormData = MimeType.Multipart matches request.getContentType

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
        http.Accepted().withBody(id)

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
