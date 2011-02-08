import util.matching.Regex

/*
* Created by IntelliJ IDEA.
* User: svetylk0@seznam.cz
* Date: 2.2.11
* Time: 22:29
*/

object BugWorkarounds {

  def w1(loginUrlReg: Regex, response: String) = loginUrlReg findFirstMatchIn response match {
    case Some(m) => (m group 1).replaceAll("\\&amp;", "&")
    case None => ""
  }
}