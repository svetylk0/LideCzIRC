
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
 * Vyjimka obecneho selhani.
 */
object Failure extends SelfThrowingException("Doslo k selhani nektere z komponent.")