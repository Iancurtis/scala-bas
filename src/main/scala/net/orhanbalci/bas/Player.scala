package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType
import akka.actor.{Actor, ActorRef, Props, ActorLogging}
import akka.io.{Tcp}
import akka.util.ByteString

class Player(id: String, connection: ActorRef)
    extends Actor
    with ActorLogging {

  import Player._
  var name = ""

  def receive = {
    case Tcp.Received(data) =>
      //log.debug(s"$id says ${data.utf8String}")
      val request = decodeIncommingMessage(data)
      log.debug(
        s"$id request type ${request.requestType} request message ${request.textMessage}")
      context.parent ! Table.PlayerMessage(request)
    //connection ! Tcp.Write(ByteString.fromString("hello from bas"))
    case Tcp.PeerClosed =>
      context.parent ! Table.UnRegister(id)
      context stop self
    case SendMessage(textMessage) =>
      connection ! Tcp.Write(
        encodeOutgoingMessage(FS_SEND_TEXT_MESSAGE,
                                     textMessage))
    case AskName =>
        connection ! Tcp.Write(encodeOutgoingMessage(FS_ASK_NAME,""))
    case SetName(playerName) =>
      name = playerName
    case SendPlayerInfo(name, relativeDirection) =>
      connection ! Tcp.Write(encodeOutgoingMessage(FS_SEND_NEW_USER_INFOS,""))

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
  case object AskName
  case class SendPlayerInfo(name : String, relativeDirection : RelativeDirection)

}
