import actors.Actor._
import actors.{OutputChannel, Actor}
import java.net.Socket

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

case class ClientState(var login: String = "",
                       var password: String = "")


object Gate extends Actor {
  //spustime Gate aktera hned ze startu
  start

  import Queries._
  import Commands._
  import collection.mutable

  //zde budeme uchovavat stavove veliciny klienta
  val clientStates = mutable.Map[Client,ClientState]()

  def getClientState(c: Client) = clientStates.lift(c) match {
    case Some(x) => x
    case None =>
      val nstate = new ClientState
      clientStates += c->nstate
      nstate
  }

  def getNickName(c: Client) = getClientState(c).login

  def answer[A <: Response](a: OutputChannel[A])(f: => A) {
    actor{
      a ! f
    }
  }

  def act {
    loop{
      react{
        //Raw zprava od klienta
        case (client: Client, line: String) =>
          //fault-tolerant -- "nechme to spadnout, protoze to stejne jednou spadne"
          try {
            //parsovat zpravu
            val RawMessage(c,mp,tp) = line
            //rozdelit parametry
            val command = c.toUpperCase
            val middleParameters = getMiddleParameters(mp)
            val tailParameter = tp

            //volani metody podle prikazu
            command match {
              case "PASS" => getClientState(client).password = middleParameters.head
              case "USER" => //zahodit, neni dulezite
              case "NICK" => client ! nick(client, getClientState(client), middleParameters)
              //case "PRIVMSG" =>
              case _ => client ! systemNotice(getNickName(client),"Neznamy prikaz: "+command+" ("+line+")")
            }


          } catch {
            //po padu poslat zpravu s informaci o chybe a znovu se zotavit
            case e: MatchError => client ! systemNotice(getNickName(client),"Nastala chyba pri parsovani prikazu: "+line)
                                  e.printStackTrace

            case e: Exception => client ! systemNotice(getNickName(client),"Nastala chyba pri zpracovani prikazu: "+line)
                                 e.printStackTrace
          }

        //pozadavek o aktualizaci textu v kanalu
        case ChannelMessagesRefresh(ch) =>
          answer(sender) {
            ChannelUsers(LideAPI.users(ch))
          }

        //synchronni dotaz na stav klienta
/*        case ClientStateRequest(c) =>
          def createNewStateAndReturn = {
            val newState = new ClientState
            clientStates += c->newState
            newState
          }

          val replyValue = if (clientStates contains c) clientStates(c)
            else createNewStateAndReturn

          reply(replyValue)*/

        case SetClientLogin(c,l) => getClientState(c).login = l

        case SetClientPassword(c,p) => getClientState(c).password = p

        case _ => //vse ostatni zahodit
      }
    }
  }
}
