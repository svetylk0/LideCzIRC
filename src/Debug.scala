
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 28.1.11
 * Time: 15:37
 * To change this template use File | Settings | File Templates.
 */

object Debug {
  var verboseEnabled = false
  var debugEnabled = false

  def verbose(f: => Unit) {
    if (verboseEnabled) f
  }

  def debug(f: => Unit) {
    if (debugEnabled) f
  }

}