package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.{*, given}

import org.{ scalatra => http }
import scala.util.{ Success, Failure }
import nostrrent.nostr.NostrSignature

import scala.jdk.CollectionConverters.given
import nostrrent.bittorrent.BTHash
import annotation.nowarn

import language.implicitConversions

trait UploadPublish:
  torrentServlet: TorrentServlet =>

  private final val FilenameParm = "filename"
  protected final val NostrrentIDParm = "nostrrentID"

  extension[AR <: http.ActionResult](result: AR)
    /** Content negotiate return formatting. */
    def withBody(id: NostrrentID | Null = null, hash: BTHash | Null = null): AR =
      val (ct, body) =
        request.getHeaders("Accept").asScala
          .flatten(_.split(",").iterator: @nowarn) // TODO: Remove @nowarn when Scala code updated
          .map(acc => MimeType(acc.trim))
          .concat(Iterator.single(MimeType.JSON)) // Default return type
          .collectFirst:
            case ct @ MimeType.JSON() => ct -> {
              id -> hash match
                case (null, null) => "{}"
                case (id, null) => s"""{"id":"$id"}"""
                case (null, hash) => s"""{"hash":"$hash"}"""
                case (id, hash) => s"""{"id":"$id","hash":"$hash"}"""
            }
            case ct @ MimeType.PlainText() => ct -> {
              id -> hash match
                case (null, null) => ""
                case (id, null) => id.toString
                case (null, hash) => s":$hash"
                case (id, hash) => s"$id:$hash"
            }
          .get
      result.copy(
        body = body,
        headers = result.headers.updated("Content-Type", ct),
      ).asInstanceOf[AR]

  protected def version: bittorrent.Version =
    import bittorrent.Version
    val v1 = params.getBoolean("v1", false)
    val v2 = params.getBoolean("v2", false)
    (v1, v2) match
      case (true, false) => Version.v1
      case (false, true) => Version.v2
      case _ => Version.hybrid

  protected def hideServer: Boolean =
    params.getBoolean("hideServer", false)

  private def getFilename(contentType: MimeType): String =
    val extLookup = FileExtensionLookup.get(contentType)
    params.get(FilenameParm)
      .map: filename =>
        val ext = extLookup || "" // if lookup fails, just accept filename as-is
        if filename.endsWith(ext) then filename
        else s"$filename$ext" // Otherwise attach correct extension type
      .getOrElse:
        // If lookup fails, use sub-type
        val ext = extLookup || s".${contentType.getSubType}"
        s"${contentType.getPrimaryType}$ext"


  protected def uploadFiles(
    appendExisting: NostrrentID | Null = null)
    : Either[http.ActionResult, NostrrentID] =

    Option(request.getContentType).map(MimeType(_)) match
      case None => Left:
        http.BadRequest("Content-Type is missing")
      case Some(contentType) =>
        val isMultipartFormData = MimeType.Multipart matches contentType
        if isMultipartFormData then Right:
          val files =
            fileMultiParams.iterator
              .flatMap(_._2)
              .map: item =>
                item.name -> item.part.getInputStream
          bt.saveFiles(files, Option(appendExisting))
        else request.getContentLength match
          case -1 => Left:
            http.LengthRequired("Content-Length missing")
          case 0 => Left:
            http.BadRequest("No content")
          case _ => Right:
            val filename = getFilename(contentType)
            val file = Iterator.single(filename -> request.getInputStream)
            bt.saveFiles(file, Option(appendExisting))

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
            .map(xsURL => magnet.copy(xs = xsURL.toURI :: Nil))
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

  // Delete unpublished upload
  delete(s"$UploadPath/:$NostrrentIDParm/?"):
    val id = NostrrentID(params(NostrrentIDParm))
    id.locked:
      bt.deleteFiles(id) match
        case None => http.NoContent()
        case Some(btHash) => http.Conflict(s"Already published: $btHash")


  put(s"$UploadPath/:$NostrrentIDParm/publish/?"):
    val id = NostrrentID(params(NostrrentIDParm))
    id.locked:
      publishFiles(id, version, hideServer)

  // Upload and publish immediately
  post(s"/publish/?"):
    uploadFiles() match
      case Left(err) => err
      case Right(id) =>
        publishFiles(id, version, hideServer)
