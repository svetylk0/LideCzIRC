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
    
    if (args contains "-v") Debug.verboseEnabled = true
    
    val server = new ServerSocket(6667)
    println("Nasloucham na portu 6667 ...")

    while(true) {
      val s = server.accept
      println("Nove pripojeni od: "+s.getInetAddress.toString)
      (new Client(s)).start
    }

  }

}