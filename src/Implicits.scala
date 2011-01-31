import java.io.{InputStreamReader, InputStream}
import scala.collection.immutable.PagedSeq


/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 30.1.11
 * Time: 16:54
 * To change this template use File | Settings | File Templates.
 */

object Implicits {

  import Globals.encoding

  implicit def compareStringIgnoreCase(str: String) = new {
    def ===(that: String) = str.equalsIgnoreCase(that)
  }

  implicit def wrapInputStream[T <: InputStream](is: T) =
    PagedSeq.fromReader(new InputStreamReader(is,encoding))

}