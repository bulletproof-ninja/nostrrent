package nostrrent.web

import nostrrent.*
import org.eclipse.jetty.server.{ Server, ServerConnector }
import org.eclipse.jetty.util.thread.ThreadPool
import org.eclipse.jetty.server.HttpConfiguration
import org.eclipse.jetty.server.HttpConnectionFactory
import org.eclipse.jetty.ee10.servlet.ServletContextHandler
import org.scalatra.util.MultiMapHeadView

import language.implicitConversions

package object jetty_scalatra:

  private val log = Logger(this.getClass)

  extension(params: MultiMapHeadView[String, String])
    def getBoolean(name: String, default: Boolean): Boolean =
      params.get(name).map(_.toLowerCase).map:
        case "false" => false
        case _ => true
      .getOrElse(default)

  private def httpConf =
    val conf = HttpConfiguration()
    conf.setSendServerVersion(false)
    // httpConf.setPersistentConnectionsEnabled()
    conf.setIdleTimeout(5000)
    conf.setSendDateHeader(false)
    conf

  private def Server(port: Int, threadPool: ThreadPool = null): Server =
    val server = new Server(threadPool)
    val connector = new ServerConnector(server, HttpConnectionFactory(httpConf))
    connector.setPort(port)
    server.addConnector(connector)
    server

  private def initSysProps(): Unit =
    System.setProperty("org.eclipse.jetty.LEVEL", "INFO"): Unit

  def run(fileSystem: LocalFileSystem, port: Int, replaceLocalhost: Option[String]): Unit =
    initSysProps()
    val server =
      // val threadPool = VirtualThreadPool()
      // threadPool.setTracking(true)   // Optional: Track virtual threads for dumps
      // threadPool.setDetailedDump(true) // Optional: Detailed thread dumps
      Server(port /*, threadPool*/)
    val rootCtx = ServletContextHandler("/")
    server.setHandler(rootCtx)

    // Add .torrent mapping:
    rootCtx.getMimeTypes().addMimeMapping("torrent", MimeType.TorrentFile)

    // Torrent content server:
    val wsServlet = WebSeedServlet("/ws", fileSystem.workDir)
    wsServlet.configure(rootCtx)

    // Torrent file server:
    val xsServlet = ExactSourceServlet("/xs", fileSystem.workDir)
    xsServlet.configure(rootCtx)

    // API spec:
    val apiServlet = APIServlet("/api")
    apiServlet.configure(rootCtx)

    val serverPaths = ServerPaths(xsPathPrefix = xsServlet.urlPath, wsPathPrefix = wsServlet.urlPath)

    // Torrent upload & seed server:
    val torrentServlet = TorrentServlet(fileSystem, serverPaths, replaceLocalhost)
    val torrentHolder = rootCtx.addServlet(torrentServlet, "/torrent/*")
    torrentServlet.init(torrentHolder)

    server.start()
    log.info("Server started!")
    server.join()
