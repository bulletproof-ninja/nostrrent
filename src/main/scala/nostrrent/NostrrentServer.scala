package nostrrent

import nostrrent.bittorrent.JLibTorrent
import java.io.File
import java.util.Properties

object NostrrentServer:

  private val log = Logger(this.getClass)

  final val PropsFile = "/nostrrent.properties"
  final val LocalPropsOverrideFile = "/nostrrent.local.properties"

  /**
    * App properties, in this priority:
    *   1. Command line `-Dnostrrent.prop=value`
    *   2. From `nostrrent.local.properties`
    *   3. From `nostrrent.properties`
    */
  val props: String => String =
    val fileProps = Properties()
    def loadProps(filename: String) =
      getClass.getResourceAsStream(filename) match
        case null => log.warn(s"No `$filename` file found in root classpath")
        case is => fileProps.load(is); is.close()
    loadProps(PropsFile); loadProps(LocalPropsOverrideFile) // local properties take priority
    (key: String) =>
      sys.props.get(s"nostrrent.$key") || {
        Option(fileProps.getProperty(key)) ||
        { throw UnknownProperty(key) }
      }

  def propsOpt(key: String): Option[String] =
    try Some(props(key))
    catch
      case UnknownProperty(_) => None

  def main(args: Array[String]): Unit =
    // Resolve config:
    val ioBufferSize = props("ioBufferSize").toInt
    val torrentDir = File(props("torrentDir")).getCanonicalFile
    val serverPort = props("serverPort").toInt
    val torrentImpl = JLibTorrent(torrentDir, ioBufferSize)

    // Use Jetty/Scalatra implementation
    web.jetty_scalatra.run(torrentImpl, serverPort, propsOpt("replaceLocalhost"))
