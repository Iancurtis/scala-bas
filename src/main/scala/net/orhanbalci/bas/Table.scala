package net.orhanbalci.bas
import akka.actor.{Actor, Props, ActorRef}
import scala.collection.mutable

class Table extends Actor {
  var players = mutable.Map[String, ActorRef]()

  override def receive = {
    case Table.Register(remote, connection) =>
  }
}

object Table {
  def props(): Props = {
    Props(new Table())
  }

  case class Register(remote: String, connection: ActorRef)
}
