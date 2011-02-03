import math.{random,round}
import java.net.URLEncoder.encode

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */


class LideAPI {
  import Globals._
  import Implicits._

  val http = new Http(userAgent,encoding)
  import http.{Get,Post}

  //konstanty
  val LoginFormUrl = "https://login.szn.cz/loginProcess"
  val ChannelContentUrlPattern = "http://chat.lide.cz/room.fcgi?akce=win_js&auth=&room_ID="
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
  val linksReg = """<a[^>]+?>(\S+?)<\/a>""".r
  val channelMapReg = "room.fcgi\\?auth=&room_ID=(\\d+)\" title=\"Vstoupit\">([^<]+)</a>".r

  val loginUrlReg = """href="([^"]+)""".r
  val leavingUrlReg = """room.fcgi\?akce=odejit&room_ID=[0-9]+&auth=&hashId=[0-9]+""".r
  val c1timeReg = """cfg.c1time\s*=\s*"(\d+)""".r
  val c2timeReg = """\"c2time" VALUE="([0-9]+)\"""".r
  val hashIdReg = """=\"hashId\" value=\"([0-9]+)\"""".r
  val channelMessageReg = """top.a4\((\d+),'\w*','([^']+)','([^']*)','([^']*)'""".r
  val channelNameReg = """Místnost:\s*<span class="red">([^<]+)</span>""".r
  val channelTopicReg = """<strong>Popis:</strong></td>\s*\n\s*<td>\s*\n\s*((\S| )+)""".r
  val channelDsReg = """Aktuální správce:[\S\s]+?<td>\s+([\S\s]+?)\s+?<\/td""".r
  val channelSsReg = """href="http://profil.lide.cz/([^/]+)/profil/" target="_blank">[^<]+</a>,""".r
  val userListReg = """<OPTION VALUE="\d+"\s*>(\S+)\s*\((m|ž)\)""".r

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
    val url = BugWorkarounds.w1(loginUrlReg,response)

    Get(url)
    response = Get("http://www.lide.cz/")
    if (!response.contains("http://profil.lide.cz/"+username)) LoginErrorException
  }

  def partChannel(id: String) {
    Get(leavingUrlMap(id))
  }

  def joinChannel(client: Client, id: String) {
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

    //ziskat potrebne udaje kanalu
    val (channelDS, channelSS, channelName, channelTopic) = channelDetails(id)

    //ziskat seznam uzivatelu
    val users = channelUsers(id)

    //vytvorit mapu privilegii
    val privileges = Map[Privilege,List[String]](DS -> List(channelDS), SS -> channelSS)

    //vytvorit Channel
    new Channel(id, channelName, channelTopic, client, privileges, users)
  }

  def channelUsers(ch: Channel): List[User] = channelUsers(ch.id)

  def channelUsers(id: String) = {
    val response = Get(ChannelTextUrlPattern + id)
    (userListReg findAllIn response).matchData map { m =>
      (m group 1, m group 2) match {
        case (nick, "ž") => User(nick, Female)
        case (nick, _) => User(nick, Male)
      }
    } toList
  }

  def removeHtmlTags(s: String) = s.replaceAll("<[^>]+?>","")

  def channelDetails(id: String) = {
    val response = Get(ChannelDetailsUrlPattern + id)

    val name = channelNameReg findFirstMatchIn response match {
      case Some(m) => m group 1
      case None =>
        ChannelNameParseFailure
        ""
    }

    val topic = channelTopicReg findFirstMatchIn response match {
      case Some(m) => m group 1
      case None =>
        ChannelNameParseFailure
        ""
    }

    val DS = channelDsReg findFirstMatchIn response match {
      case Some(m) => removeHtmlTags(m group 1)
      case None =>
        ChannelDsParseFailure
        ""
    }

    val SS = (for (m <- channelSsReg findAllIn response matchData;
                                if m.group(1) != null) yield m group 1).toList

    (DS, SS, name, topic)
  }

  def mapChannelId(name: String) = {
    //vrati Id typu string, anebo selze
    val encodedName = encode(name, "UTF-8");

    val response = Get("http://chat.lide.cz/index.fcgi?auth=&obj=room&search=" + encodedName.replaceAll("\\s+", "%20"));

    (channelMapReg findAllIn response).matchData find { m =>
      (m group 2) === name
    } match {
      case Some(m) => m group 1
      case None =>
        ChannelMapFailure
        ""
    }
    //lide.getRoomId(name)
  }

  def parseSmileys(s: String) = smileyReg replaceAllIn (s, x => "{" + (x group 2) + "}")

  def parseLinks(s: String) = linksReg replaceAllIn (s, _ group 1)

  def channelMessages(id: String) = {
    //nacteme zpravy v kanalu
    val response = Get(ChannelContentUrlPattern + id + "&m=1&" + round(random*9999))

    //prevod na objekty zprav
    (channelMessageReg findAllIn response).matchData map { m =>
      //parsovani smajliku do textove formy
      val message = parseLinks(parseSmileys(m group 2))



      //messageId, from, to
      (m group 1, m group 3, m group 4) match {
        case (messageId, "", "") => SystemMessage(messageId.toLong, "#"+id, removeHtmlTags(message))
        case (messageId, from, "") => CommonMessage(messageId.toLong, from, "#"+id, message)
        case (messageId, from, to) => WhisperMessage(messageId.toLong, from, to, message, id)
      }
    } toList
  }

  def messageConstants(id: String) = {
    val response = Get(ChannelTextUrlPattern + id)

    val c2time = c2timeReg findFirstMatchIn response match {
      case Some(m) => m group 1
      case None =>
        C2timeParseFailure
        ""
    }

    val hashId = hashIdReg findFirstMatchIn response match {
      case Some(m) => m group 1
      case None =>
        HashIdParseFailure
        ""
    }

    (c2time,hashId)
  }

  def sendMessage(id: String, message: String) {

    //nacteme konstanty potrebne pro odeslani zpravy
    val (c2time,hashId) = messageConstants(id)

    val data = Map(
      "akce" -> "text",
      "room_ID" -> id,
      "text" -> message,
      "send" -> "2",
      "skin" -> "",
      "nechat" -> "",
      "c2time" -> c2time,
      "last_ID" -> "0",
      "ID_to" -> "0",
      "mysub" -> "Poslat",
      "hashId" -> hashId,
      "auth" -> "",
      "ID_text_q" -> ""
    )

    //Odeslat zpravu
    Post(ChannelTextUrlPattern + "?" + c1timeMap(id), data)
  }
}