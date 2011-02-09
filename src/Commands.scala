import util.matching.Regex.Match

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
  val UrlReg = """\w+:\/\/[\S]+""".r

  def getMiddleParameters(p: String) = {
    val reg = """\S+""".r
    reg findAllIn p toArray
  }

  def welcomeMessageResponse(nick: String) = {
    val prefix = ":"+gateName+" 372 " + nick + " :- "
    val msgBegin =
      ":"+gateName+" 001 " + nick + " :\n" +
      ":"+gateName+" 005 " + nick + " PREFIX=(Aohv)^@%+ CHANTYPES=# :are supported by this server\n"
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

        client.api.login(user,client.password,domain)
        val nick = if (domain == "seznam.cz") user else user+"@"+domain
        //ulozit state klienta
        client.login = nick
        //poslat welcome zpravu
        client ! welcomeMessageResponse(nick)

      case None => Failure
    }
  }

  def mode(client: Client, params: Array[String]) {
    val (id, mode, target) = (params(0) drop 1, params(1), params(2))
    mode match {
      case "+o" => client.api.sendMessage(id, "/admin "+target)
      case _ => //tise ignorovat
    }
  }

  def whois(client: Client, params: Array[String]) {
    val nick = params.head
    val (channels, age, city) = client.api.profileInfo(nick)

    client ! Response(":"+gateName+" 311 "+client.login+" "+nick+" "+nick+" "+city+" * :"+nick+" ("+age+")")

    if (!channels.isEmpty) client ! Response(":"+gateName+" 319 "+client.login+" "+nick+" :" + {
      channels map { "#" + _ } mkString(" ")
    })

    client ! Response(":"+gateName+" 318 "+client.login+" "+nick+" :End of /WHOIS list.")
  }


  def who(client: Client, params: Array[String]) {
    val target = params.head
    if (target startsWith "#") client ! Response(":"+gateName+" 315 "+client.login+" "+target+" :End of /WHO list.")
  }

  def join(client: Client, params: Array[String]) {
    //projit vsechny nazvy kanalu (mohou byt oddeleny carkou) a vstoupit do nich
    for (x <- params map { _ split "," } flatten) x match {
      case RoomNameReg(channel) =>
        val id = channel match {
          case RoomIdReg(channelId) => channelId //je to Id, muzeme to pouzit primo
          case _ => client.api.mapChannelId(channel) //je to nazev, musime ho mapovat na Id
        }

        //vytvori novy kanal, ktery sam klientovi posle JOIN zpravu
        client.api.joinChannel(client,id)
      case _ => //tise ignorovat
    }
  }

  def part(client: Client, params: Array[String]) {
    val id = params.head drop 1
    client.api.partChannel(id)
    //najit kanal a poslat mu zpravu aby ukoncil cinnost
    client.channels find { _.id == id } match {
      case Some(ch) => ch ! ChannelPart
      case None => ChannelPartFailure
    }
  }

  val replacer = (m: Match) => try {
    io.Source.fromURL("http://jdem.cz/get?url="+m.toString).mkString match {
      case "http://jdem.cz/fuck" => m.toString
      case url => url
    }
  } catch {
    case _ => m.toString
  }

  def shortenURLs(client: Client, from: String, text: String) = {
    val replaced = UrlReg.replaceAllIn(text, replacer)
    //pokud se retezce lisi, oznamit zmeny jednotlivych URL
    if (text != replaced) {
      for ((a,b) <- (UrlReg.findAllIn(text)) zip (UrlReg.findAllIn(replaced))) {
        client ! noticeResponse("Jdem.cz", from, a + " -> " + b)
      }
    }
    replaced
  }

  def privmsg(client: Client, params: Array[String], tmpMsg: String) {
    //zkratit adresy pomoci jdem.cz
    val msg = shortenURLs(client, params.head, tmpMsg)

    //pokud je to kanal, odebereme # a pouzijeme jeho id
    val (id,message) = if (params.head.startsWith("#")) (params.head drop 1, msg)
      else {
        //je-li to soukroma zprava, posleme ji treba do 1. kanalu v seznamu kanalu klienta
        //a pred zpravu pridame: /m [komu]
        (client.channels.head.id, "/m "+params.head+" "+msg)
      }

    client.api.sendMessage(id,message)
  }

  def noticeResponse(from: String, to: String, text: String) = response("NOTICE", from, to, text)
  def systemNoticeResponse(to: String, text: String) = noticeResponse(gateName, to, text)

  private def response(kind: String, from: String, target: String, content: String = "") = Response(":"+from+" "+kind+" "+target+" :"+content)
}