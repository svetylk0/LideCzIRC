import math.{random,round}
import java.net.URLEncoder.encode
import actors.Futures.future
import scala.xml.Utility.Escapes.escMap

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */


class LideAPI {
  import Globals._
  import Queries._
  import Implicits._

  val http = new Http(userAgent,encoding)
  import http.{Get,Post}

  //konstanty
  val LoginFormUrl = "https://login.szn.cz/loginProcess"
  val LideChatUrl = "http://chat.lide.cz/"
  val ChannelTextUrlPattern = "http://chat.lide.cz/room.fcgi?akce=text&auth=&skin=&m=1&room_ID="
  val ChannelEntranceUrlPattern = "http://chat.lide.cz/room.fcgi?auth=&room_ID="
  val ChannelInfoUrlPattern = "http://chat.lide.cz/room.fcgi?akce=info&auth=&room_ID="
  val ChannelDetailsUrlPattern = "http://chat.lide.cz/index.fcgi?akce=info_room&auth=&room_ID="
  val CategoryUrlPattern = "http://chat.lide.cz/index.fcgi?akce=rooms&auth=&category_ID="
  val ChatUrl = "http://chat.lide.cz/"

  //vyjimky (formou metod)
  def ChannelContentUrlPattern(id: String, lastId: String) =
    LideChatUrl+"room.fcgi?akce=win_js&room_ID="+id+"&auth=&last="+lastId+"&"+round(random*9999d)

  def ProfileUrl(nick: String) = "http://profil.lide.cz/"+nick+"/profil/"

  //promenne
  //pro kazde id kanalu budeme ukladat c1time
  var c1timeMap = Map[String,String]()
  //po kazdem refreshi ulozit Url pro nacteni zprav
  var channelContentUrlMap = Map[String,String]()
  var lastIdMap = Map[String,String]()
  //URL pro odchod z kanalu
  var leavingUrlMap = Map[String,String]()

  //regularni vyrazy
  val ageReg = """<p class="age">[\S\s]+?<span.+?>(\d+)\s+let[\S\s]+?</p>""".r
  val cityReg = """<p class="age">[\S\s]+?<\/span>[\S\s]+?, (.+)[\S\s]+?</strong>\s*</p>""".r
  val c1timeReg = """cfg.c1time\s*=\s*"(\d+)""".r
  val c2timeReg = """\"c2time" VALUE="([0-9]+)\"""".r
  val hashIdReg = """=\"hashId\" value=\"([0-9]+)\"""".r
  val channelContentUrlReg = """<FRAME NAME="win" SRC="(room.fcgi\?akce=win_js&auth=&room_ID=\d+&m=1&\d+.\d+)""".r
  val channelDsReg = """Aktuální správce:[\S\s]+?<td>\s+([\S\s]+?)\s+?<\/td""".r
  val channelMapReg = "room.fcgi\\?auth=&room_ID=(\\d+)\" title=\"Vstoupit\">([^<]+)</a>".r
  val channelMessageReg = """top.a4\((\d+),'\w*','([^']+)','([^']*)','([^']*)'""".r
  val channelNameReg = """Místnost:\s*<span class="red">([^<]+)</span>""".r
  val channelsOnlineReg = """<a.+?href="http://chat.lide.cz/room.fcgi\?auth=&room_ID=(\d+)" >.+?</a>""".r
  val channelsReg = """<strong><a href="room.fcgi\?auth=&room_ID=(\d+)">(.+?)<.a><.strong>[\S\s]+?<td class="center w">(\d+)""".r
  val channelSsReg = """href="http://profil.lide.cz/([^/]+)/profil/" target="_blank">[^<]+</a>,""".r
  val channelTopicReg = """<strong>Popis:</strong></td>\s*\n\s*<td>\s*\n\s*((\S| )+)""".r
  val lastIdReg = """top.last_ID=(\d+);</script>""".r
  val leavingUrlReg = """room.fcgi\?akce=odejit&room_ID=[0-9]+&auth=&hashId=[0-9]+""".r
  val linksReg = """<a[^>]+?>(\S+?)<\/a>""".r
  val loginUrlReg = """href="([^"]+)""".r
  val smileyReg = """<img src=".+?smiles/([^.]+).gif" alt="(.+?)" height="\d+" width="\d+" />""".r
  val stateReg = """<p class="online">\s*<span>(.+?)<.span>\s*(.+?)\s*<.p>""".r
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
    /*val url = loginUrlReg findFirstMatchIn response match {
      case Some(m) => (m group 1).replaceAll("\\&amp;", "&")
      case None => ""
    } */

    Get(url)
    response = Get("http://www.lide.cz/")
    if (!response.contains("http://profil.lide.cz/"+username)) LoginErrorException
  }


  def partChannel(id: String) {
    Get(leavingUrlMap(id))
  }

  def channelState(id: String) = {
    //(re)vstup do kanalu
    channelEntranceProcedure(id)

    //ziskat potrebne udaje kanalu
    val (channelDS, channelSS, channelName, channelTopic) = channelDetails(id)

    //vytvorit mapu privilegii
    val privileges = Map(channelDS -> DS) ++ (channelSS map { _ -> SS }).toMap ++ (chatAdmins map { _ -> Admin }).toMap

    //ziskat seznam uzivatelu + pridat sebe
    val users = channelUsers(id)

    //ziskat seznam zprav
    val messages = channelMessages(id)

    ChannelState(channelName, channelTopic, messages, users, privileges)
  }

  def channelEntranceProcedure(id: String) {
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

    //pridat Url pro nacteni zprav do mapy
    channelContentUrlMap += id -> {
      channelContentUrlReg findFirstMatchIn response match {
        case Some(m) => LideChatUrl + (m group 1)
        case None => //
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
  }

  def joinChannel(client: Client, id: String) {
    //ziskat stav kanalu
    val state = channelState(id)

    //vytvorit Channel
    new Channel(id, state.name, state.topic, client, state.privileges, state.users)
  }

  def channelUsers(id: String) = {
    val response = Get(ChannelTextUrlPattern + id)

    (userListReg findAllIn response).matchData map { m =>
      (m group 1, m group 2) match {
        case (nick, "ž") => User(nick, Female)
        case (nick, _) => User(nick, Male)
      }
    } toList /*match {
      case x if x.size == 0 =>
        //jestli je seznam uzivatelu prazdny - neco je spatne
        //zkusime part&join
        Gate ! PartAndJoin(id, this)
        Nil
      case x => x
    }          */
  }

  val chatAdmins = {
    val reg = """<td>(.+?)<\/td>\s*<td>.+?<\/td>\s*<td><img""".r
    for {m <- (reg findAllIn Get("http://administrator.sweb.cz/index.php")).matchData.toList
           val nick = m group 1
           if nick != null } yield nick
  }

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

    val SS = {
      for {m <- channelSsReg findAllIn response matchData
           val nick = m group 1
           if nick != null } yield nick
    }.toList

    (DS, SS, name, topic)
  }

  def mapChannelId(name: String) = {
    //vrati Id typu string, anebo selze
    val encodedName = encode(name, "UTF-8")

    val response = Get("http://chat.lide.cz/index.fcgi?auth=&obj=room&search=" + encodedName.replaceAll("\\s+", "%20"))

    (channelMapReg findAllIn response).matchData find { m =>
      (m group 2) === name
    } match {
      case Some(m) => m group 1
      case None =>
        ChannelMapFailure
        ""
    }
  }

  def parseSmileys(s: String) = smileyReg replaceAllIn (s, x => "{" + (x group 2) + "}")

  def parseLinks(s: String) = linksReg replaceAllIn (s, _ group 1)

 
  def unescape(s: String, map: List[(Char,String)] = escMap.toList): String = map match {
    case (ch,str) :: tail => unescape(s.replaceAll(str,ch.toString),tail)
    case Nil => s
  }

  def firstMessagesUrl(id: String) = {
    val reg = """<FRAME NAME="win" SRC="(room.fcgi\?akce=win_js&auth=&room_ID=\d+&m=1&\d+\.\d+)""".r
    reg findFirstMatchIn Get(ChannelEntranceUrlPattern + id) match {
      case Some(m) => LideChatUrl + (m group 1)
      case None => ChannelContentUrlPattern(id, "0")
    }
  }

  def channelMessages(id: String) = {
    //nacteme zpravy v kanalu
//    val response = Get(channelContentUrlMap(id))
    val response = lastIdMap.lift(id) match {
      case Some(lastId) => Get(ChannelContentUrlPattern(id, lastId))
      case None => Get(firstMessagesUrl(id))
    }

    //nacteme a ulozime lastId
    lastIdReg findFirstMatchIn response match {
      case Some(m) => lastIdMap += id -> (m group 1)
      case None => LastIdParseFailure
    }

    //prevod na objekty zprav
    (channelMessageReg findAllIn response).matchData map { m =>
      //parsovani smajliku do textove formy
      //parsovat linky a HTML znaky
      val message = unescape(parseLinks(parseSmileys(m group 2)))

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

  def removeHtmlTags(s: String) = s.replaceAll("<[^>]+?>","")

  def profileInfo(nick: String) = {
    val response = Get(ProfileUrl(nick))

    val channels = (channelsOnlineReg findAllIn response).matchData.toList map {
      _ group 1
    }

    val age = ageReg findFirstMatchIn response match {
      case Some(m) => (m group 1)+" let"
      case None => "vek neuveden"
    }

    val city = {
      cityReg findFirstMatchIn response match {
        case Some(m) => m group 1
        case None => "mesto neni uvedeno"
      }
    } replaceAll (" ","_")

    val state = stateReg findFirstMatchIn response match {
      case Some(m) => (m group 1)+" "+removeHtmlTags(m group 2)
      case None => "nepodarilo se nacist stav"
    }

    (channels, state, age, city)
  }

  def channelList = {
    val listOfResponses = for(id <- 1 to 8) yield future {
      Get(CategoryUrlPattern+id)
    }

    val data = listOfResponses map { _() } mkString

    (channelsReg findAllIn data).matchData.toList map { m =>
      (m group 1, m group 2, m group 3)
    }
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