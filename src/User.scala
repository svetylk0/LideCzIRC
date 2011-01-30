
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 18:09
 * To change this template use File | Settings | File Templates.
 */

trait Privilege

object Ordinary extends Privilege
object DS extends Privilege
object SS extends Privilege
object Admin extends Privilege

trait Gender

object Male extends Gender
object Female extends Gender

case class User(val nick: String, val gender: Gender = Male, var mod: Privilege = Ordinary)

