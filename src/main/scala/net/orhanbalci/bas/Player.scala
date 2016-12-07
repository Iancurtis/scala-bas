package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType
import akka.actor.{Actor, ActorRef, Props, ActorLogging}
import akka.io.{Tcp}
import akka.util.ByteString

class Player(id: String, connection: ActorRef)
    extends Actor
    with ActorLogging {

  var name = ""

  def receive = {
    case Tcp.Received(data) =>
      //log.debug(s"$id says ${data.utf8String}")
      val request = Player.decodeIncommingMessage(data)
      log.debug(
        s"$id request type ${request.requestType} request message ${request.textMessage}")
      context.parent ! Table.PlayerMessage(request)
    //connection ! Tcp.Write(ByteString.fromString("hello from bas"))
    case Tcp.PeerClosed =>
      context.parent ! Table.UnRegister(id)
      context stop self
    case Player.SendMessage(textMessage) =>
      connection ! Tcp.Write(
        Player.encodeOutgoingMessage(RequestResponseType.FS_SEND_TEXT_MESSAGE,
                                     textMessage))
    case Player.SetName(playerName) =>
      name = playerName

  }
}

object Player {
  def props(id: String, connection: ActorRef): Props = {
    Props(new Player(id, connection))
  }

  def decodeIncommingMessage(data: ByteString): BasRequestResponse = {
    val dataArray = data.toArray
    BasRequestResponse.parseFrom(dataArray)
  }

  def encodeOutgoingMessage(messageType: RequestResponseType,
                            textMessage: String): ByteString = {
    val outgoing = BasRequestResponse(requestType = messageType,
                                      textMessage = textMessage,
                                      errorCode = 1)
    ByteString.fromArray(outgoing.toByteArray)
  }

  case class SendMessage(messageText: String)
  case class SetName(name: String)

}
