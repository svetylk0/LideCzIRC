import actors.Actor._
import actors.Actor
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

object Gate extends Actor {
  import Queries._
  import Responses._
  import Commands._

  //spustime Gate aktera hned ze startu
  start

  def guardedActor(client: Client)(f: => Unit) {
    actor {
      guarded(client)(f)
    }
  }

  def guarded(client: Client)(f: => Unit) {
    try {
      f
    } catch {
//      case e: Exception => client ! systemNoticeResponse(client.login, "Nastala chyba: "+e.getMessage)
      case e: Exception => client ! errorResponse("Nastala chyba: "+e.getMessage)
    }
  }

  def act {
    loop {
      react {
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
              case "PASS" => pass(client,middleParameters,tailParameter)
              case "USER" => //zahodit, neni dulezite
              case "NICK" => //prihlaseni necham jako blokujici, protoze meni stav klienta (promennou login)
                nick(client, middleParameters)
              case "WHO" => who(client,middleParameters)

              case "WHOIS" => guardedActor(client) {
                whois(client,middleParameters)
              }

              case "MODE" => guardedActor(client) {
                mode(client, middleParameters)
              }

              case "LIST" => guardedActor(client) {
                list(client)
              }

              case "JOIN" => guardedActor(client) {
                join(client, middleParameters)
              }

              case "PART" => guardedActor(client) {
                part(client, middleParameters)
              }

              case "KICK" => guardedActor(client) {
                privmsg(client, middleParameters, "/kick "+tailParameter)
              }

              case "ADMINS" => guardedActor(client) {
                client ! systemNoticeResponse(client.login, "Admini online: "+client.api.onlineAdmins.mkString(", "))
              }

              case "QUIT" => client.channels foreach {
                _ ! ChannelPart
              }

              case "PRIVMSG" => guardedActor(client) {
                privmsg(client, middleParameters, tailParameter)
              }

              case _ => client ! systemNoticeResponse(client.login,"Neznamy prikaz: "+command+" ("+line+")")
            }


          } catch {
            //po padu poslat zpravu s informaci o chybe a znovu se zotavit
            case e: MatchError => client ! systemNoticeResponse(client.login,"Nastala chyba pri parsovani prikazu: "+line)
                                  e.printStackTrace

            case e: Exception => client ! systemNoticeResponse(client.login,"Nastala chyba pri zpracovani prikazu: "+line)
                                 e.printStackTrace
          }

        //pozadavek o aktualizaci stavu v kanalu
        case ChannelStateRefresh(ch) => actor {
          ch ! ch.client.api.channelState(ch.id)
        }

        case _ => //vse ostatni zahodit
      }
    }
  }
}
