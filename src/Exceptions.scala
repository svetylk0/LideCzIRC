
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 28.1.11
 * Time: 14:20
 * To change this template use File | Settings | File Templates.
 */

abstract class SelfThrowingException(str: String) extends Exception(str) {
  throw this
}

object Failure extends SelfThrowingException("Failure")