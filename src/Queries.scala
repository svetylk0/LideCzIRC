
/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 18:54
 * To change this template use File | Settings | File Templates.
 */

object Queries {
  //dotazy, ktere posila Channel na Gate
  case class ChannelMessagesRefresh(channel: Channel)
  case class ChannelUsersRefresh(channel: Channel)



  //obecne dotazy
  case class ClientStateRequest(client: Client)
  case class SetClientPassword(client: Client, pass: String)
  case class SetClientLogin(client: Client, login: String)

  //odpovedi pro klienta
  trait Response

  case class StringResponse(item: String) extends Response
  case class EmptyResponse() extends Response

  //odpovedi, ktere dostava Channel od Gate
  case class ChannelUsers(users: List[User]) extends Response
  case class ChannelMessages(users: List[Message]) extends Response

}