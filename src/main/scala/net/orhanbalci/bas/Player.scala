package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType._
import net.orhanbalci.bas.protocol.BasRequestResponse.{
  RequestResponseType,
  UserDirection
}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString

import scala.collection.mutable

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
        encodeOutgoingMessage(messageType = FS_SEND_TEXT_MESSAGE,
                              textMessage = textMessage))
    case AskName =>
      connection ! Tcp.Write(encodeOutgoingMessage(messageType = FS_ASK_NAME))
    case SetName(playerName) =>
      name = playerName
    case SendPlayerInfo(name, relativeDirection) =>
      connection ! Tcp.Write(
        encodeOutgoingMessage(messageType = FS_SEND_NEW_USER_INFOS,
                              userDirection = relativeDirection))
    case SendAllPlayerInfos(directionNameMap) =>
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

  def encodeOutgoingMessage(
      messageType: RequestResponseType = Unrecognized(-1),
      textMessage: String = "",
      userDirection: RelativeDirection = SelfDirection): ByteString = {
    val outgoing = BasRequestResponse(
      requestType = messageType,
      textMessage = textMessage,
      errorCode = 1,
      userDirection = UserDirection.fromValue(userDirection.code))
    ByteString.fromArray(outgoing.toByteArray)
  }

  case class SendMessage(messageText: String)
  case class SetName(name: String)
  case object AskName
  case class SendPlayerInfo(name: String, relativeDirection: RelativeDirection)
  case class SendAllPlayerInfos(
      nameMap: mutable.Map[RelativeDirection, String])

}
