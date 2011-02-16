import Globals._

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
  import Responses._

  //udalosti, ktere jsou odesilany klientovi
  case class JoinEvent(user: User, channel: Channel) extends ResponseLike {
    def toResponse = Response(":" + user +" JOIN :#" + channel.id)
  }

  case class PartEvent(user: User, channel: Channel) extends ResponseLike {
    def toResponse = Response(":" + user +" PART :#" + channel.id)
  }

  case class MessageEvent(x: Message) extends ResponseLike {
    def toResponse = x.toResponse
  }

  case class VoiceUserEvent(user: User, channel: Channel) extends ResponseLike {
    def toResponse = Response(":"+gateName+" MODE #" + channel.id + " +v " + user.nick)
  }

  case class PrivilegeChangeEvent(user: User,
                                  channel: Channel,
                                  lastPrivilege: Privilege,
                                  newPrivilege: Privilege) extends ResponseLike {

    def toResponse = (lastPrivilege, newPrivilege) match {
      case (Ordinary, Admin) => Response(":"+gateName+" MODE #" + channel.id + " +A " + user.nick)
      case (Ordinary, SS) => Response(":"+gateName+" MODE #" + channel.id + " +o " + user.nick)
      case (Ordinary, DS) => Response(":"+gateName+" MODE #" + channel.id + " +h " + user.nick)
      case (DS, Ordinary) => Response(":"+gateName+" MODE #" + channel.id + " -h " + user.nick)
      case _ => systemNoticeResponse("#"+channel.id, "Nastala neznama zmena prav uzivatele: "+user.nick)
    }


  }

}