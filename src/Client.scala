import actors.Actor
import actors.Actor._
import concurrent.ThreadRunner
import java.io.{BufferedWriter, BufferedReader, OutputStreamWriter, InputStreamReader}
import java.net.Socket



/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

class Client(val socket: Socket) extends Actor {
  import Globals._
  import Debug._
  import Queries._

  val reader = new BufferedReader(new InputStreamReader(socket.getInputStream, encoding))
  val writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, encoding))

  val channels = List[Channel]()

  //login a heslo klienta
  var login = ""
  var password = ""


  def dispose {
    exit
  }

  def disposeOnException(f: => Unit) {
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

  def read: String = reader.readLine

  //cteni vstupu klienta a preposilani  Gate
  actor {
    disposeOnException {
      while(true) {
        val line = read

        verbose {
          println("<<< "+line)
        }

        if (line == null) dispose
        else Gate ! ((this,line))
      }
    }
  }

  //reakce na prichozi zpravy z Gate
  def act {
    loop {
      react {
        case Response(msg: String) =>
          verbose {
            println(">>> "+msg)
          }

          write(msg+"\n")


        case r: ResponseLike => self ! r.toResponse

        case _ => println("Prijata neznama zprava.")
      }
    }
  }

}