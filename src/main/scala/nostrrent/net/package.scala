package nostrrent

import java.net.{ InetAddress, DatagramSocket }
import scala.util.{ Using, Try }

package object net:

  def OutgoingAddresses: List[InetAddress] =
    val (host, port) = "one.one.one.one" -> 53
    Using.resource(DatagramSocket()): socket =>
      InetAddress.getAllByName(host)
        .flatMap: addr =>
          Try:
            socket.connect(addr, port)
            try socket.getLocalAddress finally socket.disconnect()
          .toOption
        .toList.distinct
