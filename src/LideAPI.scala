
/**
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 24.1.11
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */

object LideAPI {
  import Queries._

  def users(ch: Channel): Option[List[User]] = {
    Some(List(User("pan1",Male,Ordinary),
    User("adminka",Female,Admin),
    User("DS",Male,DS),
    User("SSka",Female,SS)))
  }
}