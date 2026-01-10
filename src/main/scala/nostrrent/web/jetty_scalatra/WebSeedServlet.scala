package nostrrent.web.jetty_scalatra

import java.io.File

/**
  * Torrent content server (magnet link `ws` parameter).
  * @param path server path prefix
  * @param workDir Root torrent dir
  */
class WebSeedServlet(val path: String, workDir: File)
extends AbstractFileServer(AbstractFileServer.fileSystemDir(workDir))
