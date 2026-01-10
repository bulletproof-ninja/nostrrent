package nostrrent.web.jetty_scalatra

import org.{scalatra => http}, http.ScalatraServlet
import scala.util.control.NonFatal
import java.time.format.DateTimeParseException
import jakarta.servlet.ServletException
import jakarta.servlet.http.{
  HttpServletRequest, HttpServletResponse
}
import java.nio.charset.StandardCharsets.UTF_8
import java.net.URLDecoder

trait BaseServlet(replaceLocalhost: Option[String])
extends ScalatraServlet:

  protected val log = nostrrent.Logger(this.getClass)

  protected def requestURL()(using HttpServletRequest) =
    URLDecoder.decode(summon[HttpServletRequest].getRequestURL.toString, UTF_8)

  private val serverNameMapping: String => Option[String] = replaceLocalhost match
    case None => _ => None
    case Some(replacement) => Map("localhost" -> replacement).get

  override def service(request: HttpServletRequest, response: HttpServletResponse): Unit =
    val replaceReq = new ReplaceServerNameRequest(request, serverNameMapping)
    super.service(replaceReq, response)

  override def log(msg: String): Unit = log(msg, null)
  override def log(message: String, t: Throwable): Unit =
    t match
      case null => log.info(message)
      case t => log.warn(message, t)

  protected def errMsg(th: Throwable): String =
    if th == null then "Unknown error"
    else th.getMessage match
      case null | "" => errMsg(th.getCause)
      case msg => msg

  // Don't return Content-Type when no body
  override protected def contentTypeInferrer: org.scalatra.ContentTypeInferrer =
    case () | null | "" => null
    case body => super.contentTypeInferrer(body)

  notFound:
    log(s"Not found: ${request.getRequestURL}")
    http.NotFound("N/A", Map("Connection" -> "close"))

  error:
    case e @ (
      _: IllegalArgumentException |
      _: DateTimeParseException) =>
      http.BadRequest(errMsg(e))

    case e: IllegalStateException =>
      http.Conflict(errMsg(e))

    case e: ServletException =>
      http.BadRequest(errMsg(e.getCause))

    case NonFatal(th) =>
      log("Non-fatal exception", th)
      http.InternalServerError()
