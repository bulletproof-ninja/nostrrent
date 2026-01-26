package nostrrent.web.jetty_scalatra

import java.io.File
import jakarta.servlet.http.{ HttpServletRequest, HttpServletResponse }
import nostrrent.TorrentFileExt

/**
  * Torrent content server (magnet link `ws` parameter).
  * @param path server path prefix
  * @param workDir Root torrent dir
  */
class WebSeedServlet(val urlPath: String, workDir: File)
extends AbstractFileServer(workDir):

  import HttpServletResponse.*

  override def doGet(request: HttpServletRequest, response: HttpServletResponse): Unit =
    val filename = request.getPathInfo.drop(1)
    if filename.endsWith(TorrentFileExt) && ! filename.contains("/") then
      response.sendError(SC_NOT_FOUND)
    else
      super.doGet(request, response)
