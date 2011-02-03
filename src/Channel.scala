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

trait Disposable {
  def dispose {
    exit
  }
}

trait Idler {
  import System.{currentTimeMillis => time}

  val idleTime = 60*1000*30l
  val idlerString = "."

  var lastTime = time

  def idlerReset {
    lastTime = time
  }

  def diff = time - lastTime

  def idlerCheckout(execute: => Unit) {
    if (diff > idleTime) {
      execute
      idlerReset
    }
  }
}

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
              val client: Client,
              privilegedUsers: Map[Privilege,List[String]],
              var users: List[User]) extends Actor with Disposable with Idler {

  //ozivit instanci kanalu hned po vytvoreni
  start

  val textRefreshTimeout = 10000l
  val usersSynchronizationTimeout = 1000l*15l
  val nickname = client.login

  var lastMessageId = 0l
  var alive = true

  /**
   * vykonat pri vytvoreni instance kanalu
   */

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

  /**
   * konec bloku
   */

  def refreshPrivileges = applyPrivileges(users) { (u,privilege) =>
    client ! PrivilegeChangeEvent(u, this, u.mod, privilege)
  }

  def applyPrivileges(userList: List[User])(reaction: (User,Privilege) => Unit) {
    for ((privilege,ulist) <- privilegedUsers; nick <- ulist) {
      userList find {
        _.nick === nick
      } match {
        case Some(u) =>
          if (u.mod != privilege) {
            //oznamit zmenu prav klientovi pres funkci reakce
            reaction(u,privilege)
            u.mod = privilege
          }
        case None =>
      }
    }
  }

  //pomocna funkce, ktera nedela nic (zmeny prav se nebudou oznamovat)
  val noPrivilegeNotification = (a: User, b: Privilege) => ()

  def updateUserChanges(list: List[User]) {
    //nastavime prava uzivatelu v novem listu a az pak porovnavame a oznamime zmeny
    applyPrivileges(list)(noPrivilegeNotification)

    //v obou pripadech ignorujeme sebe
    //oznamit odchod
    users filterNot { n => list.exists { _.nick === n.nick } } foreach { u =>
      if (u.nick !== nickname) client ! PartEvent(u,this)
    }

    //oznamit prichod
    list filterNot { n => users.exists { _.nick === n.nick } } foreach { u =>
      if (u.nick !== nickname) {
        client ! JoinEvent(u,this)
        //pokud je uzivatel zena, pridat ji voice mod
        if (u.gender == Female) client ! VoiceUserEvent(u,this)
      }
    }

    //ulozit aktualni stav
    users = list

    //aktualizovat prava uzivatelu
    refreshPrivileges
  }


  def processNewMessages(list: List[Message]) {
    //vybrat nove zpravy
    val newMessages = list filter {
      _.id > lastMessageId
    }

    //pokud je nejaka nova zprava od nas, resetovat idler
    if (newMessages exists { _.from === client.login }) idlerReset

    //provest idler checkout (jestli nebyla prekrocena doba neaktivity)
    //pokud ano, poslat . na kanal a zaroven oznameni klientovi
    idlerCheckout {
      Gate ! ((client,"PRIVMSG #"+id+" :"+idlerString))
      client ! MessageEvent(SystemMessage(0,"#"+id,"Idler:"+idlerString))
    }

    //odeslat je klientovi + odfiltrovat zpravy, ktere pochazi od nas
    for (message <- newMessages if message.from !== client.login) client ! MessageEvent(message)
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

        case ChannelPart =>
          //poslat udalost odchodu
          client ! PartEvent(User(client.login),this)
          //odhlasit kanal u klienta
          client ! ChannelRemoval(this)
          //ukoncit beh refreshujiciho aktera
          refreshActor.dispose
          //ukoncit svuj beh
          dispose
        case _ => //vse ostatni zahodit
      }
    }
  }

  //uzivatele a zpravy budeme refreshovat najednou (a casto)
  //akter, ktery bude refreshovat zpravy v kanalu
  val parentChannel = this
  val refreshActor = new Actor() with Disposable {
    start

    def act {
      while(true) {
        Gate ! ChannelMessagesRefresh(parentChannel)
        Gate ! ChannelUsersRefresh(parentChannel)
        Thread.sleep(textRefreshTimeout)
      }
    }
  }

}

