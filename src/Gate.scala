import actors.Actor
import java.net.Socket

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:07
 * To change this template use File | Settings | File Templates.
 */

class Gate extends Actor {
  import Queries._

  def answer(a: Actor)(f: => Any) {
    actor {
      a ! f
    }
  }

  def act {
    loop {
      react {
        //pozadavek o aktualizaci textu v kanalu
        case ChannelTextRefresh(ch) => answer(sender) {
          LideAPI.users(ch) match {
            case Some(list) => list
            case None => println("Nepodarilo se nacist zpravy z kanalu: "+ch.name)
          }
        }

      }
    }
  }
}
