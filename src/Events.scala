

/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 25.1.11
 * Time: 18:14
 * To change this template use File | Settings | File Templates.
 */

object Events {
  //udalosti, ktere jsou odesilany klientovi
  case class JoinEvent(user: User)
  case class MessageEvent(message: Message)
  case class PartEvent(user: User)
  case class PrivilegeChangeEvent(user: User, privilege: Privilege)

}