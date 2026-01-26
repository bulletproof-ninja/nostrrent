package nostrrent.web.jetty_scalatra

import org.eclipse.jetty.ee10.servlet.ResourceServlet
import jakarta.servlet.http.{ HttpServletRequest, HttpServletResponse }
import java.io.File
import org.eclipse.jetty.ee10.servlet.ServletContextHandler
import org.eclipse.jetty.util.resource.{ ResourceFactory, Resource }

abstract class AbstractFileServer(getBase: ResourceFactory => Resource)
extends ResourceServlet:

  def this(absDir: File) = this(AbstractFileServer.fileSystemDir(absDir))

  protected def urlPath: String

  protected def servletConfigParms: Map[String, Any] =
    Map(
      "acceptRanges" -> true,
      "cacheControl" -> "public, immutable",
      "cacheValidationTime" -> -1,
      "dirAllowed" -> false, // We *could* override this for the collection dirs and serve JSON
      "maxCachedFiles" -> 0,
      "useFileMappedBuffer" -> false, // Seems memory inefficient, *particularly* for large files, and have GC issues
    )

  def configure(ctx: ServletContextHandler): Unit =
    val holder = ctx.addServlet(this, s"$urlPath/*")
    val rf = ResourceFactory.of(ctx)
    val servletConfigParms = Map(
      "baseResource" -> getBase(rf),
    )
    (servletConfigParms ++ this.servletConfigParms)
      .foreach: (key, value) =>
        holder.setInitParameter(key, String.valueOf(value))

  /**
   * Make 404 into 403 to match directory listing attempt.
   * This minimizes information leaking about server state.
   */
  override def doNotFound(req: HttpServletRequest, res: HttpServletResponse, encodedPathInContext: String): Unit =
    res.sendError(HttpServletResponse.SC_FORBIDDEN)

object AbstractFileServer:
  private def fileSystemDir(workDir: File)(rf: ResourceFactory): Resource =
    rf.newResource(workDir.toPath)
