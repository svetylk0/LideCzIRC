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

  def users(ch: Channel) = lide.getRoomUsers(ch.name).toList map { x =>
    User(x.getNick, Male, Ordinary)
  }

  val loginUrlReg = """href="([^"]+)""".r
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
    val url = loginUrlReg findFirstMatchIn response match {
      case Some(m) => (m group 1).replaceAll("\\&amp;", "&")
      case None => ""
    }

    Get(url)
    response = Get("http://www.lide.cz/")
    if (!response.contains("http://profil.lide.cz/"+username)) LoginErrorException
  }

  def joinChannel(client: Client, id: String) = {
    //vstoupit
    lide.enterTheRoom(id)

    //ziskat potrebne udaje
    val info = lide.getRoomInfo(id)
    val users = lide.getRoomUsers(id).toList map { u =>
      val gender = if (u.isMale) Male else Female
      User(u.getNick, gender)
    }

    val privileges = Map[Privilege,List[String]](DS -> List(info.getDS), SS -> info.getSS.toList)

    //vrati Channel
    new Channel(id, info.getName, info.getTopic, client, privileges, users)
  }

  def mapChannelId(name: String) = {
    //vrati Id typu string, anebo selze
    lide.getRoomId(name)
  }

  val smileyReg = """<img src=".+?smiles/([^.]+).gif" alt="(.+?)" height="\d+" width="\d+" />""".r
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