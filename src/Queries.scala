
/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 18:54
 * To change this template use File | Settings | File Templates.
 */

object Queries {
  case class ChannelTextRefresh(channel: Channel)
  case class ChannelUsers(users: List[User])
}