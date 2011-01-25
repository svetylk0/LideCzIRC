import actors.Actor

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:40
 * To change this template use File | Settings | File Templates.
 */

trait Message

case class CommonMessage(id: Long,
                         from: Option[String],
                         text: String) extends Message

case class WhisperMessage(id: Long,
                          from: Option[String],
                          to: Option[String],
                          text: String) extends Message

case class SystemMessage(id: Long, text: String) extends Message

class Channel(val id: Long,
              val name: String,
              val topic: String,
              client: Actor,
              privilegedUsers: Map[Privilege,List[String]],
              var users: List[User]) extends Actor {

  import Globals.threadRunner
  import Queries._
  import Commands._
  import Events._

  val textRefreshTimeout = 5000l
  val usersSynchronizationTimeout = 1000l*60l

  def refreshPrivileges {
    for ((privilege,ulist) <- privilegedUsers; user <- ulist) users find {
        _.nick == user.nick
    } match {
      case Some(u) =>
        if (u.mod != privilege) {
          u.mod = privilege
          //oznamit zmenu prav klientovi
          client ! PrivilegeChangeEvent(u,privilege)
        }
      case None =>
    }
  }

  //aktualizovat prava uzivatelu hned na zacatku
  refreshPrivileges

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

  var lastMessageId = 0

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

  //vlakno, ktere bude refreshovat zpravy v kanalu
  threadRunner execute {
    loop {
      Thread.sleep(textRefreshTimeout)
      Gate ! ChannelMessagesRefresh(this)
    }
  }

  //vlakno, ktere synchronizuje uzivatele kanalu
  threadRunner execute {
    loop {
      Thread.sleep(usersSynchronizationTimeout)
      Gate ! ChannelUsersRefresh(this)
    }
  }



}

