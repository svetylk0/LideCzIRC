import jlidegw.Lide
import collection.JavaConversions._
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */

object LideAPI {

  val lide = new Lide

  def users(ch: Channel) = lide.getRoomUsers(ch.name).toList map { x =>
    User(x.getNick, Male, Ordinary)
  }

  def login(u: String, p: String, d: String) = lide.login(u,p,d)

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
        case (f: String,to: String) => WhisperMessage(m.getId.toLong, m.getFrom, m.getTo, message)
        case _ => SystemMessage(m.getId.toLong, "#"+id, message)
      }
    }
  }
}