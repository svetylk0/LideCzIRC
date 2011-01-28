import jlidegw.Lide
import collection.JavaConversions._
/**
 * Created by IntelliJ IDEA.
 * Author: svetylk0@seznam.cz
 * Date: 24.1.11
 * Time: 14:06
 * To change this template use File | Settings | File Templates.
 */

object LideAPI {

  val lide = new Lide


  def users(ch: Channel) = lide.getRoomUsers(ch.name).toList map { x =>
    User(x.getNick, Male, Ordinary)
  }

  def login(u: String, p: String, d: String) = lide.login(u,p,d)
}