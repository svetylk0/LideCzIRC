/*
 * Created by IntelliJ IDEA.
 * User: hacx
 * Date: 2.2.11
 * Time: 11:38
 */

object Responses {

  val accessDeniedReg = """"<P>Byl jste vykopnut.+<span class=\"red\">.+</span></P> <P>Vzkazuje.+<span class=\"red\">.+</span></P>""".r

  //overi jestli mame pristup do mistnosti
  def checkForAccessDeniedResponse(response: String) {
    accessDeniedReg findFirstIn response match {
      case Some(m) => new AccessDeniedException(m)
      case None =>
    }
  }

}