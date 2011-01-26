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

  def disposeOnException[A](f: => A) {
    try {
      f
    } catch {
      case e: Exception => e.printStackTrace
                           dispose
    }
  }

  def write(data: String) {
    disposeOnException {
      writer.write(data)
      writer.flush
    }
  }

  def read = disposeOnException {
    reader.readLine
  }

  //cteni vstupu klienta a preposilani  Gate
  threadRunner execute {
    loop {
      val line = read
      if (line == null) dispose
      else Gate ! ((this,line))
    }
  }

  //reakce na prichozi zpravy z Gate
  def act {
    loop {
      react {

      }
    }
  }

}