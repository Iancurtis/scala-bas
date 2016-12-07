package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType._
import akka.actor.{Actor, Props, ActorRef, ActorLogging}
import akka.io.{Tcp}
import scala.collection.mutable

class Table extends Actor with ActorLogging {
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
    case Table.PlayerMessage(playerRequest) =>
      handlePlayerMessage(playerRequest, sender)
  }

  def handlePlayerMessage(playerRequest: BasRequestResponse,
                          senderPlayer: ActorRef) = {
    playerRequest.requestType match {
      case FC_SEND_TEXT_MESSAGE =>
        sendUserMessage(senderPlayer, playerRequest.textMessage)
      case FC_SEND_NAME =>
        setPlayerName(senderPlayer, playerRequest.name)
    }

  }

  def sendUserMessage(sender: ActorRef, message: String) = {
    players.foreach(player =>
      if (player._2 != sender) { player._2 ! Player.SendMessage(message) })
  }

  def setPlayerName(sender: ActorRef, name: String) = {
    sender ! Player.SetName(name)
  }

}

object Table {
  def props(): Props = {
    Props(new Table())
  }

  case class Register(remote: String, connection: ActorRef)
  case class UnRegister(playerId: String)
  case class PlayerMessage(playerRequest: BasRequestResponse)
}
