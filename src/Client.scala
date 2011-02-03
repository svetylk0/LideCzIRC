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
  import Events._

  val reader = new BufferedReader(new InputStreamReader(socket.getInputStream, encoding))
  val writer = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, encoding))

  val api = new LideAPI

  var channels = List[Channel]()

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

        case ChannelRegistration(ch) => channels ::= ch

        case ChannelRemoval(ch) => channels = channels filterNot { _.id == ch.id }

        //soukrome zpravy odchytavat zvlast a brat je pouze z jedineho kanalu (protoze jsou replikovany
        //do vsech kanalu soucasne) - pouzijeme treba vzdy 1. kanal v seznamu
        case MessageEvent(msg @ WhisperMessage(_,_,_,_,channelId)) =>
          //pokud je zprava z 1. kanalu v seznamu, preposlat ji jako odpoved ke zpracovani
          //jinak zahodit
          if (channelId == channels.head.id) self ! msg.toResponse

        //odchytit vse, co lze konvertovat na Response a preposlat ji ke zpracovani
        case r: ResponseLike => self ! r.toResponse
        case _ => println("Prijata neznama zprava.")
      }
    }
  }

}