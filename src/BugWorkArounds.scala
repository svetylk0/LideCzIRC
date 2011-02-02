import util.matching.Regex

/*
* Created by IntelliJ IDEA.
* User: hacx
* Date: 2.2.11
* Time: 9:45
*/
object BugWorkArounds {

  def w1(loginUrlReg: Regex, response: String) = loginUrlReg findFirstMatchIn response match {
    case Some(m) => (m group 1).replaceAll("\\&amp;", "&")
    case None => ""
  }


}