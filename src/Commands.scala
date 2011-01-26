

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 25.1.11
 * Time: 18:11
 * To change this template use File | Settings | File Templates.
 */

object Commands {
  val RawMessage = """:?(\w+)([^:]+):([^\n\r]+)?""".r


  def pass(c: Client, passwd: String) {
    val state = Gate !! ClientStateRequest(c)
    Gate ! UpdateClientState(c,state().copy(password = passwd))
  }

  def nick(c: Client, params: Array[String]) {

  }

  def notice(user: String, text: String) = ":"+user+" NOTICE :"+text
  def systemNotice(text: String) = notice("Gate",text)

}