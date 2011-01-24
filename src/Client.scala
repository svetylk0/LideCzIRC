import actors.Actor
import concurrent.ThreadRunner
import java.io.{BufferedWriter, BufferedReader, OutputStreamWriter, InputStreamReader}
import java.net.Socket



/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

class Client(val socket: Socket) extends Actor {
  import Globals._

  val reader = new BufferedReader(InputStreamReader(socket.getInputStream, encoding))
  val writer = new BufferedWriter(OutputStreamWriter(socket.getOutputStream, encoding))

  val channels = List[Channel]()

  def dispose {
    exit
  }

  def write(data: String) {
    writer.write(data)
    writer.flush
  }

  def read = reader.readLine

  //cteni vstupu klienta a preposilani  Gate
  threadRunner execute {
    loop {
      val line = read
      if (line == null) dispose
      else Gate ! line
    }
  }

}