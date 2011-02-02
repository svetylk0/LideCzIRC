
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 28.1.11
 * Time: 14:20
 * To change this template use File | Settings | File Templates.
 */

/**
 * Abstraktni trida definujici vyjimku, ktera se po vytvoreni jeji instance
 * sama vyhodi.
 */
abstract class SelfThrowingException(str: String) extends Exception(str) {
  throw this
}

/**
 * Vyjimky se zpravou
 */
class AccessDeniedException(s: String) extends SelfThrowingException(s)


/**
 * Vyjimka obecneho selhani.
 */
object Failure extends SelfThrowingException("Doslo k selhani nektere z komponent.")

object C1timeParseFailure extends SelfThrowingException("Selhalo parsovani hodnoty c1time.")
object LeavingUrlParseFailure extends SelfThrowingException("Selhalo parsovani URL pro opusteni kanalu.")
object ChannelNameParseFailure extends SelfThrowingException("Selhalo parsovani nazvu kanalu.")
object ChannelDsParseFailure extends SelfThrowingException("Selhalo parsovani docasneho spravce kanalu.")


