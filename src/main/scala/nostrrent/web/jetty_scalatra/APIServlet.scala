package nostrrent.web.jetty_scalatra

import org.eclipse.jetty.util.resource.{ Resource, ResourceFactory }

final class APIServlet(val path: String)
extends AbstractFileServer(APIServlet.baseDir(path))

object APIServlet:
  def baseDir(path: String)(rf: ResourceFactory): Resource =
    rf.newClassLoaderResource(s"/web$path", false)
