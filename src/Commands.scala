

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
      "LideCzIRC",
      "",
      "Vitejte!","","") map { prefix + _ + "\n"}

    Response(msgBegin + lines.mkString + msgEnd)
  }

  def pass(client: Client, params: Array[String], tail: String) {
    client.password = if (tail == null) params.head else tail
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

  def join(client: Client, params: Array[String]) {
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

  def part(client: Client, params: Array[String]) {
    val id = params.head drop 1
    LideAPI.partChannel(id)
    //najit kanal a poslat mu zpravu aby ukoncil cinnost
    client.channels find { _.id == id } match {
      case Some(ch) => ch ! ChannelPart
      case None => ChannelPartFailure
    }
  }

  def privmsg(client: Client, params: Array[String], msg: String) {
    //pokud je to kanal, odebereme # a pouzijeme jeho id
    val (id,message) = if (params.head.startsWith("#")) (params.head drop 1, msg)
      else {
        //je-li to soukroma zprava, posleme ji treba do 1. kanalu v seznamu kanalu klienta
        //a pred zpravu pridame: /m [komu]
        (client.channels.head.id, "/m "+params.head+" "+msg)
      }

    LideAPI.sendMessage(id,message)
  }

  def noticeResponse(from: String, to: String, text: String) = response("NOTICE", from, to, text)
  def systemNoticeResponse(to: String, text: String) = noticeResponse(gateName, to, text)

  private def response(kind: String, from: String, target: String, content: String = "") = Response(":"+from+" "+kind+" "+target+" :"+content)
}