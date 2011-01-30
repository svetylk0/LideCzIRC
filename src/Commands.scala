

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

  //regularni vyrazy
  val RawMessage = """:?(\w+)([^:]+):?([^\n\r]+)?""".r
  val NickReg = """([^@]+)(@(\S+))?""".r
  val RoomNameReg = """#(\S+)""".r
  val RoomIdReg = """(\d+)""".r

  def getMiddleParameters(p: String) = {
    val reg = """\S+""".r
    reg findAllIn p toArray
  }

  def welcomeMessageResponse(nick: String) = {
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
    NickReg.findFirstMatchIn(params.head) match {
      case Some(m) =>
        val domain = if (m.group(2) == null) "seznam.cz" else m.group(2)
        val user = m group 1

        LideAPI.login(user,client.password,domain)
        val nick = if (domain == "seznam.cz") user else user+"@"+domain
        //ulozit state klienta
        client.login = nick
        //poslat welcome zpravu
        client ! welcomeMessageResponse(nick)

      case None => Failure
    }
  }

  def join(client: Client, params: Array[String]) = {
    //projit vsechny nazvy kanalu (mohou byt oddeleny carkou) a vstoupit do nich
    for (x <- params map { _ split "," } flatten) x match {
      case RoomNameReg(channel) =>
        val id = channel match {
          case RoomIdReg(channelId) => channelId //je to Id, muzeme to pouzit primo
          case _ => LideAPI.mapChannelId(channel) //je to nazev, musime ho mapovat na Id
        }

        //vytvori novy kanal, ktery sam klientovi posle JOIN zpravu
        LideAPI.joinChannel(client,id)
      case _ => Failure
    }
  }

  def noticeResponse(from: String, to: String, text: String) = response("NOTICE", from, to, text)
  def systemNoticeResponse(to: String, text: String) = noticeResponse(gateName, to, text)

  private def response(kind: String, from: String, target: String, content: String = "") = Response(":"+from+" "+kind+" "+target+" :"+content)
}