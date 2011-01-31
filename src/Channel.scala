import actors.Actor
import actors.Actor._


import Queries._
import Commands._
import Events._
import Globals._
import Implicits._

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:40
 * To change this template use File | Settings | File Templates.
 */

trait Message extends ResponseLike {
  val id: Long
  val from: String
  val to: String
  val text: String

  val messageType = "PRIVMSG"

  def toResponse = Response(":"+from+" "+messageType+" " + to + " :"+text)
}

case class CommonMessage(val id: Long,
                         val from: String,
                         val channel: String,
                         val text: String) extends Message {
  val to = channel
}

case class WhisperMessage(val id: Long,
                          val from: String,
                          val to: String,
                          val text: String,
                          channelId: String) extends Message

case class SystemMessage(val id: Long,
                         val channel: String,
                         val text: String) extends Message {
  val to = channel
  val from = gateName

  override val messageType = "NOTICE"
}

class Channel(val id: String,
              val name: String,
              val topic: String,
              client: Client,
              privilegedUsers: Map[Privilege,List[String]],
              var users: List[User]) extends Actor {

  //ozivit instanci kanalu hned po vytvoreni
  start

  val textRefreshTimeout = 5000l
  val usersSynchronizationTimeout = 1000l*60l
  val nickname = client.login

  //vykonat pri vytvoreni instance kanalu
  {
    //registrovat kanal u klienta
    client ! ChannelRegistration(this)

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

    //aktualizovat prava uzivatelu az na konec
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
      client ! PartEvent(_,this)
    }

    //oznamit prichod
    list filterNot { users.contains } foreach {
      client ! JoinEvent(_,this)
    }

    //ulozit aktualni stav
    users = list

    //aktualizovat prava uzivatelu
    refreshPrivileges
  }

  var lastMessageId = 0l

  def processNewMessages(list: List[Message]) {
    //vybrat nove zpravy
    // + odfiltrovat zpravy, ktere pochazi od nas
    val newMessages = list filter {
      _.id > lastMessageId
    } filterNot {
      _.from === client.login
    }

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
    while(true) {
      Gate ! ChannelMessagesRefresh(this)
      Thread.sleep(textRefreshTimeout)
    }
  }

  //akter, ktery synchronizuje uzivatele kanalu
  actor {
    while(true) {
      Thread.sleep(usersSynchronizationTimeout)
      Gate ! ChannelUsersRefresh(this)
    }
  }



}

