import actors.Actor
import actors.Actor._


import Queries._
import Commands._
import Events._
import Globals._
import Implicits._
import Debug._

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
              beginningPrivileges: Map[String,Privilege],
              var users: List[User]) extends Actor with Disposable with Idler {

  //ozivit instanci kanalu hned po vytvoreni
  start

  val textRefreshTimeout = 10000l
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
  refreshPrivileges(users, beginningPrivileges)

  /**
   * konec bloku
   */

  def refreshPrivileges(userList: List[User], privileges: Map[String,Privilege]) =
    applyPrivileges(userList, privileges) { (u,privilege) =>
      //ignorovat prechody DS -> SS a SS -> DS
      (u.mod, privilege) match {
        case (SS,DS) => //ignorovat
        case (DS,SS) => //ignorovat
        case _ => client ! PrivilegeChangeEvent(u, this, u.mod, privilege)
      }
    }

  //pomocna funkce, ktera nedela nic (zmeny prav se nebudou oznamovat)
  implicit val noPrivilegeNotification = (a: User, b: Privilege) => ()

  def applyPrivileges(userList: List[User], privileges: Map[String,Privilege])(implicit reaction: (User, Privilege) => Unit) = {
    val lifted = privileges.lift
    userList foreach { user =>
      lifted(user.nick) match {
        case Some(privilege) =>
          if (user.mod != privilege) {
            //oznamit reakci na zmenu (tato cast kontroluje pridavani prav)
            reaction(user, privilege)
            //aplikovat zmenu
            user.mod = privilege
          }
        case None =>
          //tato cast kontroluje odebirani prav
          if (user.mod != Ordinary) {
            //oznamit reakci
            reaction(user, Ordinary)
            //ulozit zmenu
            user.mod = Ordinary
          }
      }
    }
  }

  def updateUserChanges(l: List[User], privileges: Map[String,Privilege]) {
    //seznam neobsahuje nas; pridame se
    val list = User(client.login) :: l

    //v obou pripadech ignorujeme sebe
    //oznamit odchod
    users filterNot { n => list.exists { _.nick === n.nick } } foreach { u =>
      if (u.nick !== nickname) client ! PartEvent(u,this)
    }

    //prichozi
    val incoming = list filterNot { n => users.exists { _.nick === n.nick } }

    //oznamit prichod
    incoming foreach { u =>
      if (u.nick !== nickname) {
        client ! JoinEvent(u,this)
        //pokud je uzivatel zena, pridat ji voice mod
        if (u.gender == Female) client ! VoiceUserEvent(u,this)
      }
    }

    //aktualizovat prava prichozich
    refreshPrivileges(incoming, privileges)

    //nastavit prava vsech uzivatelu noveho listu a ulozit ho do users
    applyPrivileges(list, privileges)
    users = list
  }


  def processNewMessages(list: List[Message]) {

    //zpravy by mely chodit pouze nove ... prozatim vypneme
    val newMessages = list
    /*
    //vybrat nove zpravy
    val newMessages = list filter {
      _.id > lastMessageId
    } /*filterNot {
      //odfiltrovat systemove zpravy naseho odchodu a prichodu
      case SystemMessage(_, _, text) if (text.containsIgnoreCase("vstoupil") || text.containsIgnoreCase("opustil"))
        && text.containsIgnoreCase(client.login) => true
      case _ => false
    }   */ //docasne vypnout, at vim coto dela
    */

    //pokud je nejaka nova zprava od nas, resetovat idler
    if (newMessages exists { _.from === client.login }) idlerReset

    //provest idler checkout (jestli nebyla prekrocena doba neaktivity)
    //pokud ano, poslat . na kanal a zaroven oznameni klientovi
    idlerCheckout {
      Gate ! ((client,"PRIVMSG #"+id+" :"+idlerString))
      client ! MessageEvent(SystemMessage(0,"#"+id,"Idler:"+idlerString))
    }

    //odeslat nove zpravy klientovi + odfiltrovat zpravy, ktere pochazi od nas
    for (message <- newMessages if message.from !== client.login) {
      client ! MessageEvent(message)
    }
    //ulozit posledni Id, pokud je seznam neprazdny
    if (!newMessages.isEmpty) lastMessageId = newMessages.last.id
  }

  //uzivatele a zpravy budeme refreshovat najednou (a casto)
  //akter, ktery bude refreshovat zpravy v kanalu
  val parentChannel = this
  val refreshActor = new Actor() with Disposable {
    start

    def act {
      while(alive) {
        Gate ! ChannelStateRefresh(parentChannel)
        Thread.sleep(textRefreshTimeout)
      }
    }
  }

  def act {
    loop {
      react {
        case ChannelState(_, _, messages, newUsers, privileges) =>
          //refresh privilegii stavajicich
          refreshPrivileges(users, privileges)
          //uprava zmen uzivatelu a refresh privilegii prichozich
          //+ update users
          updateUserChanges(newUsers, privileges)
          processNewMessages(messages)

        case ChannelDispose =>
          //ukoncit beh refreshujiciho aktera
          alive = false
          //ukoncit svuj beh
          dispose

        case ChannelPart =>
          //poslat udalost odchodu
          client ! PartEvent(User(client.login),this)
          //odhlasit kanal u klienta
          client ! ChannelRemoval(this)
          //ukoncit cinnost
          self ! ChannelDispose
        case _ => //vse ostatni zahodit
      }
    }
  }



}

