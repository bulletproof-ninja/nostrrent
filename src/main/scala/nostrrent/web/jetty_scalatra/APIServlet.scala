package nostrrent.web.jetty_scalatra

import org.eclipse.jetty.util.resource.{ Resource, ResourceFactory }

final class APIServlet(val urlPath: String)
extends AbstractFileServer(APIServlet.baseDir(urlPath))

object APIServlet:
  def baseDir(path: String)(rf: ResourceFactory): Resource =
    rf.newClassLoaderResource(s"/web$path", false)
