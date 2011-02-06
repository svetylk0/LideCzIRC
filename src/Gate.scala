import actors.Actor._
import actors.{OutputChannel, Actor}
import java.net.Socket

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

//case class ClientState(var login: String = "",
  //                     var password: String = "")


object Gate extends Actor {
  //spustime Gate aktera hned ze startu
  start

  import Queries._
  import Commands._

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
              case "PASS" => pass(client,middleParameters,tailParameter)
              case "USER" => //zahodit, neni dulezite
              case "NICK" => //prihlaseni necham jako blokujici, protoze meni stav klienta (promennou login)
                nick(client, middleParameters)
              case "WHO" => who(client,middleParameters)
              case "MODE" => actor {
                mode(client, middleParameters)
              }
              case "JOIN" => actor {
                join(client, middleParameters)
              }
              case "PART" => actor {
                part(client, middleParameters)
              }
              case "PRIVMSG" => actor {
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
