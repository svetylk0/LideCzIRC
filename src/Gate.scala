import actors.Actor
import java.net.Socket

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

case class ClientState(login: String = "",
                       password: String = "")


object Gate extends Actor {

  import Queries._
  import Commands._
  import collection.mutable

  //zde budeme uchovavat stavove veliciny klienta
  val clientStates = mutable.Map[Client,ClientState]()


  def answer(a: Actor)(f: => Any) {
    actor{
      a ! f
    }
  }

  def act {
    loop{
      react{
        //String raw zprava od klienta
        case (client: Client, line: String) =>
          answer(sender) {
            try {
              //parsovat zpravu
              val RawMessage(c,mp,tp) = line
              //rozdelit parametry
              val command = c.toUpperCase
              val middleParameters = mp split " "
              val tailParameter = tp

              //volani metody podle prikazu
              command match {
                case "PASS" => pass(client, middleParameters.head)
                case "USER" => //zahodit, neni dulezite
                case "NICK" => nick(client, middleParameters)
                case "PRIVMSG" =>
                case _ => systemNotice("Nenalezena metoda pro prikaz: "+command+" ("+line+")")
              }


            } catch {
              case e: MatchError => systemNotice("Nastala chyba pri parsovani prikazu: "+line)
              case e: Exception => systemNotice("Nastala chyba pri zpracovani prikazu: "+line)
            }
          }

        //pozadavek o aktualizaci textu v kanalu
        case ChannelMessagesRefresh(ch) =>
          answer(sender) {
            LideAPI.users(ch) match {
              case Some(list) => ChannelUsers(list)
              case None => println("Nepodarilo se nacist zpravy z kanalu: " + ch.name)
            }
          }

        //asynchronni dotaz na stav klienta
        case ClientStateRequest(c) =>
          def createNewStateAndReturn = {
            val newState = new ClientState
            clientStates += newState
            newState
          }

          val replyValue = if (clientStates contains c) clientStates(c)
            else createNewStateAndReturn

          reply(replyValue)

        case _ => //vse ostatni zahodit
      }
    }
  }
}
