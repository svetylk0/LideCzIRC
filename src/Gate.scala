import actors.Actor
import java.net.Socket

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

object Gate extends Actor {

  import Queries._

  def answer(a: Actor)(f: => Any) {
    actor{
      a ! f
    }
  }

  def act {
    loop{
      react{
        //String raw zprava od klienta
        case line: String =>

        //pozadavek o aktualizaci textu v kanalu
        case ChannelMessagesRefresh(ch) =>
          answer(sender) {
            LideAPI.users(ch) match {
              case Some(list) => ChannelUsers(list)
              case None => println("Nepodarilo se nacist zpravy z kanalu: " + ch.name)
            }
          }

        case _ => //vse ostatni zahodit
      }
    }
  }
}
