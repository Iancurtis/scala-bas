package net.orhanbalci.bas
import akka.actor.{Actor, Props, ActorRef, ActorLogging}
import akka.io.{Tcp}
import scala.collection.mutable

class Table extends Actor with ActorLogging{
  var players = mutable.Map[String, ActorRef]()

  override def receive = {
    case Table.Register(remote, connection) =>
      val playerActor = context.actorOf(Player.props(remote, connection))
      players += (remote -> playerActor)
      connection ! Tcp.Register(playerActor)
      context.parent ! Room.UpdatePlayerCount(players.size)
    case Table.UnRegister(playerId) =>
      players -= playerId
      context.parent ! Room.UpdatePlayerCount(players.size)
  }
}

object Table {
  def props(): Props = {
    Props(new Table())
  }

  case class Register(remote: String, connection: ActorRef)
  case class UnRegister(playerId: String)
}
