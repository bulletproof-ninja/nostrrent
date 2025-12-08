package nostrrent.web.jetty_scalatra

import java.io.File

final class APIServlet(val path: String)
extends AbstractFileServer(APIServlet.workDir(path))

object APIServlet:
  def workDir(path: String): File =
    File(this.getClass.getResource(s"/web$path").getFile)
    .getCanonicalFile()
