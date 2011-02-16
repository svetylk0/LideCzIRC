/*
 * Created by IntelliJ IDEA.
 * User: svetylk0@seznam.cz
 * Date: 2.2.11
 * Time: 11:38
 */


//odpovedi pro klienta
case class Response(item: String)

object Responses {
  import Globals._

  val accessDeniedReg = """"<P>Byl jste vykopnut.+<span class=\"red\">.+</span></P> <P>Vzkazuje.+<span class=\"red\">.+</span></P>""".r

  //overi jestli mame pristup do mistnosti
  def checkForAccessDeniedResponse(response: String) {
    accessDeniedReg findFirstIn response match {
      case Some(m) => new AccessDeniedException(m)
      case None =>
    }
  }

  private def response(kind: String, from: String, target: String, content: String = "") = Response(":"+from+" "+kind+" "+target+" :"+content)
  //private def numericResponse(num: Int)

  def errorResponse(msg: String) = Response("ERROR :"+msg)

  def noticeResponse(from: String, to: String, text: String) = response("NOTICE", from, to, text)
  def systemNoticeResponse(to: String, text: String) = noticeResponse(gateName, to, text)

}