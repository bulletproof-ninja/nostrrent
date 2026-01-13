package nostrrent.web.jetty_scalatra

import java.io.File
import jakarta.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nostrrent.*, web.{MimeType, given}

/**
  * Torrent file server (magnet link `xs` parameter).
  * @param path server path prefix
  * @param workDir Root torrent dir
  */
class ExactSourceServlet(val path: String, workDir: File)
extends AbstractFileServer(AbstractFileServer.fileSystemDir(workDir)):

  import HttpServletResponse.*

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit =
    val filename = request.getPathInfo.drop(1)
    if filename.endsWith(TorrentFileExt) && ! filename.contains("/") then
      response.setContentType(MimeType.TorrentFile)
      super.doGet(request, response)
    else
      response.sendError(SC_NOT_FOUND)
