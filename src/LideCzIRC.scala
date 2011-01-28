import java.net.ServerSocket

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 28.1.11
 * Time: 15:27
 * To change this template use File | Settings | File Templates.
 */

object LideCzIRC {

  def main(args: Array[String]) {
    val server = new ServerSocket(6667)
    println("Listening on 6667 ...")

    while(true) {
      val s = server.accept
      println("Got some from: "+s.getInetAddress.toString)
      (new Client(s)).start
    }

  }

}