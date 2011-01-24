import actors.Actor

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:40
 * To change this template use File | Settings | File Templates.
 */

case class Message(id: Long, from: Option[String], to: Option[String], text: String)

class Channel(val id: Long,
              val name: String,
              val topic: String,
              client: Actor,
              var users: List[User]) extends Actor {

  import Globals.threadRunner
  import Queries._

  val timeout = 5000l

  def act {
    loop {
      react {

      }
    }
  }

  //vlakno, ktere bude refreshovat zpravy v kanalu
  threadRunner execute {
    loop {
      Thread.sleep(timeout)
      Gate ! ChannelTextRefresh(this)
    }
  }
}

