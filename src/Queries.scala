
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 18:54
 * To change this template use File | Settings | File Templates.
 */

object Queries {
  //dotazy, ktere posila Channel na Gate
  case class ChannelStateRefresh(channel: Channel)
  case class ChannelMessagesRefresh(channel: Channel)
  case class ChannelUsersRefresh(channel: Channel)

  //zpravy, ktere posila Channel na Client
  case class ChannelRegistration(channel: Channel)
  case class ChannelRemoval(channel: Channel)

  //obecne dotazy

  //odpovedi pro klienta
  case class Response(item: String)

  //odpovedi, ktere dostava Channel od Gate
  case class ChannelState(messages: List[Message],
                          users: List[User],
                          privileges: Map[Privilege,List[String]])

  case class ChannelUsers(users: List[User])
  case class ChannelMessages(messages: List[Message])
  object ChannelPart
  object ChannelDispose
}