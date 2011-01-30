package


/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 30.1.11
 * Time: 16:54
 * To change this template use File | Settings | File Templates.
 */

object Implicits {
  implicit def compareStringIgnoreCase(str: String) = new {
    def ===(that: String) = str.equalsIgnoreCase(that)
  }
}