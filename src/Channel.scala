import actors.Actor
import actors.Actor._

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:40
 * To change this template use File | Settings | File Templates.
 */

trait Message {
  val id: Long
}

case class CommonMessage(val id: Long,
                         from: Option[String],
                         text: String) extends Message

case class WhisperMessage(val id: Long,
                          from: Option[String],
                          to: Option[String],
                          text: String) extends Message

case class SystemMessage(val id: Long, text: String) extends Message

class Channel(val id: String,
              val name: String,
              val topic: String,
              client: Client,
              privilegedUsers: Map[Privilege,List[String]],
              var users: List[User]) extends Actor {

  import Queries._
  import Commands._
  import Events._
  import Globals._
  import Implicits._

  val textRefreshTimeout = 5000l
  val usersSynchronizationTimeout = 1000l*60l
  val nickname = client.login

  //vykonat pri vytvoreni instance kanalu
  {
    //pokud nejsme v seznamu, pridame se
    if (!users.exists(_.nick === nickname)) users ::= User(nickname)

    val userListMessage = users map { u =>
      if (u.gender == Male) u.nick
      else "+" + u.nick
    } mkString " "

    //po vytvoreni kanalu poslat klientovi JOIN zpravu
    client ! Response(":" + nickname + " JOIN #" + id)
    client ! Response(":"+gateName+" 332 " + nickname + " #" + id + " :[" + name + "] " + topic)
    client ! Response(":"+gateName+" NOTICE #" + id + " :URL mistnosti: http://chat.lide.cz/room.fcgi?room_ID=" + id)
    client ! Response(":"+gateName+" 353 " + nickname + " = #" + id + " :" + userListMessage)
    client ! Response(":"+gateName+" 366 " + nickname + " #" + id + " :End of /NAMES list.")

    //aktualizovat prava uzivatelu
    refreshPrivileges
  }

  def refreshPrivileges {
    for ((privilege,ulist) <- privilegedUsers; nick <- ulist) {
      users find {
        _.nick === nick
      } match {
        case Some(u) =>
          if (u.mod != privilege) {
            //oznamit zmenu prav klientovi
            client ! PrivilegeChangeEvent(u, this, u.mod, privilege)
            u.mod = privilege
          }
        case None =>
      }
    }
  }


  def updateUserChanges(list: List[User]) {
    //oznamit odchod
    users filterNot { list.contains } foreach {
      client ! PartEvent(_)
    }

    //oznamit prichod
    list filterNot { users.contains } foreach {
      client ! JoinEvent(_)
    }

    //ulozit aktualni stav
    users = list

    //aktualizovat prava uzivatelu
    refreshPrivileges
  }

  var lastMessageId = 0l

  def processNewMessages(list: List[Message]) {
    //vybrat nove zpravy
    val newMessages = list filter { _.id > lastMessageId }
    //odeslat je klientovi
    newMessages foreach { client ! MessageEvent(_)}
    //ulozit posledni Id, pokud je seznam neprazdny
    if (!newMessages.isEmpty) lastMessageId = newMessages.last.id
  }

  def act {
    loop {
      react {
        //aktualizovany seznam uzivatelu
        case ChannelUsers(list) => updateUserChanges(list)

        //aktualizovany seznam zprav v kanalu
        case ChannelMessages(list) => processNewMessages(list)

        case _ => //vse ostatni zahodit
      }
    }
  }

  //akter, ktery bude refreshovat zpravy v kanalu
  actor {
    loop {
      Thread.sleep(textRefreshTimeout)
      Gate ! ChannelMessagesRefresh(this)
    }
  }

  //akter, ktery synchronizuje uzivatele kanalu
  actor {
    loop {
      Thread.sleep(usersSynchronizationTimeout)
      Gate ! ChannelUsersRefresh(this)
    }
  }



}

