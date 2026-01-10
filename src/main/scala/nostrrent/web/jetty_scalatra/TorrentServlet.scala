package nostrrent.web.jetty_scalatra

import nostrrent.*
import nostrrent.web.*

import org.scalatra.servlet.{ FileUploadSupport }
import org.eclipse.jetty.ee10.servlet.ServletHolder
import org.{ scalatra => http }
import jakarta.servlet.http.HttpServletRequest
import jakarta.servlet.MultipartConfigElement
import java.net.{ URL, URI }

class TorrentServlet(
  protected val bt: Bittorrent,
  protected val serverPaths: ServerPaths,
  replaceLocalhost: Option[String])
extends BaseServlet(replaceLocalhost)
with FileUploadSupport
with UploadPublish
with SignedPublish
with Seed:

  def init(holder: ServletHolder): Unit =
    holder.getRegistration.setMultipartConfig(MultipartConfigElement(TempDir.getAbsolutePath))

  protected def makeURL(pathPrefix: String, leafPath: String)(using request: HttpServletRequest): URL =
    val reqURL = request.getRequestURL()
    val removePos = reqURL.indexOf("/", reqURL.indexOf("//") + 2)
    reqURL.replace(removePos, reqURL.length, "")
    reqURL.append(pathPrefix).append('/').append(leafPath)
    URI(reqURL.toString).toURL

  get("/*"):
    log(s"Invalid GET: ${request.getRequestURL}")
    http.MethodNotAllowed("N/A", Map("Connection" -> "close"))
