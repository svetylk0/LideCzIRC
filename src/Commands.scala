

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 25.1.11
 * Time: 18:11
 * To change this template use File | Settings | File Templates.
 */

object Commands {
  import actors.Actor._
  import Queries._
  import Globals.gateName

  val RawMessage = """:?(\w+)([^:]+):?([^\n\r]+)?""".r

  def getMiddleParameters(p: String) = {
    val reg = """\S+""".r
    reg findAllIn p toArray
  }

  def sendWelcomeMessage(nick: String) = {
    val prefix = ":"+gateName+" 372 " + nick + " :- "
    val msgBegin = ":"+gateName+" 001 " + nick + " :\n"
    val msgEnd = ":"+gateName+" 376 " + nick + " :End of /MOTD command."

    val lines = List("","",
      "LideCzIRC (no version available)",
      "",
      "Welcome!","","") map { prefix + _ + "\n"}

    Response(msgBegin + lines.mkString + msgEnd)
  }

  def nick(client: Client, params: Array[String]) = {
    val nickReg = """([^@]+)(@(\S+))?""".r

    nickReg.findFirstMatchIn(params.head) match {
      case Some(m) =>
        val domain = if (m.group(2) == null) "seznam.cz" else m.group(2)
        val user = m group 1

        LideAPI.login(user,client.password,domain)
        val nick = if (domain == "seznam.cz") user else user+"@"+domain
        //ulozit state klienta
        client.login = nick
        //Gate ! SetClientLogin(client,nick)
        //poslat welcome zpravu
        sendWelcomeMessage(nick)

      case None =>
    }
  }

  def notice(from: String, to: String, text: String) = Response(":"+from+" NOTICE "+to+" :"+text)

  def systemNotice(to: String, text: String) = notice(gateName, to, text)

}