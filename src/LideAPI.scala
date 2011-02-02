import jlidegw.Lide
import collection.JavaConversions._
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */
object LoginErrorException extends SelfThrowingException("Chybny login nebo heslo.")

object LideAPI {
  import Globals._

  val http = new Http(userAgent,encoding)
  val lide = new Lide

  import http.{Get,Post}

  //konstanty
  val LoginFormUrl = "https://login.szn.cz/loginProcess"
  val ChannelTextUrlPattern = "http://chat.lide.cz/room.fcgi?akce=text&auth=&skin=&m=1&room_ID="
  val ChannelEntranceUrlPattern = "http://chat.lide.cz/room.fcgi?auth=&room_ID="
  val ChannelInfoUrlPattern = "http://chat.lide.cz/room.fcgi?akce=info&auth=&room_ID="
  val ChannelDetailsUrlPattern = "http://chat.lide.cz/index.fcgi?akce=info_room&auth=&room_ID="
  val ChatUrl = "http://chat.lide.cz/"

  //promenne
  //pro kazde id kanalu budeme ukladat c1time
  var c1timeMap = Map[String,String]()
  //URL pro odchod z kanalu
  var leavingUrlMap = Map[String,String]()

  //regularni vyrazy
  val smileyReg = """<img src=".+?smiles/([^.]+).gif" alt="(.+?)" height="\d+" width="\d+" />""".r

  val loginUrlReg = """href="([^"]+)""".r
  val leavingUrlReg = """room.fcgi\?akce=odejit&room_ID=[0-9]+&auth=&hashId=[0-9]+""".r
  val c1timeReg = """cfg.c1time\s*=\s*"(\d+)""".r
  val channelNameReg = """Místnost:\s*<span class="red">([^<]+)</span>""".r
  val channelTopicReg = """<strong>Popis:</strong></td>\s*\n\s*<td>\s*\n\s*((\S| )+)""".r
  val channelDsReg = """<strong>Aktuální správce:</strong></td>\s*\n\s*<td>\s*\n\s*<a href="http://profil.lide.cz/([^/]+)""".r
  val channelSsReg = """href="http://profil.lide.cz/([^/]+)/profil/" target="_blank">[^<]+</a>,""".r
  val userListReg = """<OPTION VALUE="\d+"\s*>(\S+)\s*\((m|ž)\)""".r

  def users(ch: Channel) = lide.getRoomUsers(ch.name).toList map { x =>
    User(x.getNick, Male, Ordinary)
  }


  def login(username: String, password: String, domain: String) {

    val data = Map(
      "username" -> username,
      "domain" -> domain,
      "password" -> password,
      "remember" -> "",
      "login" -> "Přihlásit se",
      "serviceId" -> "lide",
      "disableSSL" -> "0",
      "forceSSL" -> "0",
      "lang" -> "cz",
      "loginType" -> "seznam",
      "returnURL" -> "http://www.lide.cz/",
      "forceRelogin" -> "0",
      "coid" -> ""
    )

    var response = Post(LoginFormUrl, data)
    val url = BugWorkArounds.w1(loginUrlReg,response)

    Get(url)
    response = Get("http://www.lide.cz/")
    if (!response.contains("http://profil.lide.cz/"+username)) LoginErrorException
  }

  def joinChannel(client: Client, id: String) = {
    //vstoupit do kanalu
    var response = Get(ChannelEntranceUrlPattern + id)

    //overi, jestli nebyl odmitnut pristup do kanalu
    //pokud ano, vyhodi vyjimku
    Responses.checkForAccessDeniedResponse(response)

    //pridat c1time do mapy
    c1timeMap += id -> {
      c1timeReg findFirstMatchIn response match {
        case Some(m) => m group 1
        case None => //C1timeParseFailure
          ""
      }
    }

    //otevrit stranku s info o kanalu (nutne pro dokonceni vstupu)
    response = Get(ChannelInfoUrlPattern + id)

    //ulozit URL pro odchod z kanalu
    leavingUrlMap += id -> {
      leavingUrlReg findFirstIn response match {
        case Some(m) => ChatUrl + m
        case None => //LeavingUrlParseFailure
         ""
      }
    }




    //zeby 1x na sucho?
    //channelDetails(roomID);


    //ziskat potrebne udaje kanalu
    val (channelDS, channelSS, channelName, channelTopic) = channelDetails(id)

    //ziskat seznam uzivatelu
    val users = channelUsers(id)

    //vytvorit mapu privilegii
    val privileges = Map[Privilege,List[String]](DS -> List(channelDS), SS -> channelSS)

    //vratit Channel
    new Channel(id, channelName, channelTopic, client, privileges, users)
  }

  def channelUsers(id: String) = {
    val response = Get(ChannelTextUrlPattern + id)
    (userListReg findAllIn response).matchData map { m =>
      (m group 1, m group 2) match {
        case (nick, "ž") => User(nick, Female)
        case (nick, _) => User(nick, Male)
      }
    } toList
  }

  def channelDetails(id: String) = {
    val response = Get(ChannelDetailsUrlPattern + id)

    val name = channelNameReg findFirstMatchIn response match {
      case Some(m) => m group 1
      case None => //ChannelNameParseFailure
        ""
    }

    val topic = channelTopicReg findFirstMatchIn response match {
      case Some(m) => m group 1
      case None => //ChannelNameParseFailure
        ""
    }

    val DS = channelDsReg findFirstMatchIn response match {
      case Some(m) =>
        val value = m group 1
        if (value == "není") "" else value
      case None => //ChannelDsParseFailure
        ""
    }

    val SS = (for (m <- channelSsReg findAllIn response matchData;
                                if m.group(1) != null) yield m group 1).toList

    (DS, SS, name, topic)
  }

  def mapChannelId(name: String) = {
    //vrati Id typu string, anebo selze
    lide.getRoomId(name)
  }

  def channelMessages(id: String) = {

    lide.getRoomMessages(id).toList map { m =>
      //parsovani smajliku do textove formy
      val message = smileyReg replaceAllIn (m.getText, x => "{" + (x group 2) + "}")

      (m.getFrom,m.getTo) match {
        case (f: String,null) => CommonMessage(m.getId.toLong, m.getFrom, "#"+id, message)
        case (f: String,to: String) => WhisperMessage(m.getId.toLong, m.getFrom, m.getTo, message, id)
        case _ => SystemMessage(m.getId.toLong, "#"+id, message)
      }
    }
  }

  def sendMessage(id: String, message: String) {
    lide.sendMessage(id,message)
  }
}