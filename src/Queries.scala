
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


  //odpovedi, ktere dostava Channel od Gate
  case class ChannelUsers(users: List[User])
  case class ChannelMessages(users: List[Message])

  //obecne dotazy
  case class UpdateClientState(client: Client, state: ClientState)

  //asynchronni dotazy na Gate
  case class ClientStateRequest(client: Client)

}