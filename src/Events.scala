import Queries.Response

/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 25.1.11
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */
trait ResponseLike {
  def toResponse: Response
}

object Events {
  import Commands._
  import Globals._

  //udalosti, ktere jsou odesilany klientovi
  case class JoinEvent(user: User)
  case class MessageEvent(message: Message)
  case class PartEvent(user: User)

  case class PrivilegeChangeEvent(user: User,
                                  channel: Channel,
                                  lastPrivilege: Privilege,
                                  newPrivilege: Privilege) extends ResponseLike {

    def toResponse = (lastPrivilege, newPrivilege) match {
      case (Ordinary, SS) => Response(":"+gateName+" MODE #" + channel.id + " +o " + user.nick)
      case (Ordinary, DS) => Response(":"+gateName+" MODE #" + channel.id + " +h " + user.nick)
      case (DS, Ordinary) => Response(":"+gateName+" MODE #" + channel.id + " -h " + user.nick)
      //case (SS, Ordinary) => Response(":"+gateName+" MODE #" + channel.id + " -o " + user.nick)
      case _ => systemNoticeResponse("#"+channel.id, "Nastala neznama zmena prav uzivatele: "+user.nick)
    }


  }

}