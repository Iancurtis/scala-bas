package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType._
import net.orhanbalci.bas.protocol.BasRequestResponse.{RequestResponseType, UserDirection}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp
import akka.util.ByteString

import scala.collection.mutable

class Player(id: String, connection: ActorRef) extends Actor with ActorLogging {

  import Player._
  var name = ""

  def receive = {
    case Tcp.Received(data) =>
      //log.debug(s"$id says ${data.utf8String}")
      val request = decodeIncommingMessage(data)
      log.debug(s"$id request type ${request.requestType} request message ${request.textMessage}")
      context.parent ! Table.PlayerMessage(request)
    //connection ! Tcp.Write(ByteString.fromString("hello from bas"))
    case Tcp.PeerClosed =>
      context.parent ! Table.UnRegister(id)
      context stop self
    case SendMessage(textMessage) =>
      connection ! Tcp.Write(
        encodeOutgoingMessage(messageType = FS_SEND_TEXT_MESSAGE, textMessage = textMessage))
    case AskName =>
      connection ! Tcp.Write(encodeOutgoingMessage(messageType = FS_ASK_NAME))
    case SetName(playerName) =>
      name = playerName
    case SendPlayerInfo(name, relativeDirection) =>
      connection ! Tcp.Write(
        encodeOutgoingMessage(messageType = FS_SEND_NEW_USER_INFOS,
                              userDirection = relativeDirection))
    case SendAllPlayerInfos(directionNameMap) =>
      sendAllPlayerInfos(directionNameMap)
    case SendPlayerCards(cards) =>
      sendPlayerCards(cards)
    case AskPlayCount =>
      sendAskPlayCount
    case AskTrump =>
      askTrump
  }

  def askTrump = {
    connection ! Tcp.Write(encodeOutgoingMessage(messageType = FS_ASK_TRUMP))
  }

  def sendAskPlayCount = {
    connection ! Tcp.Write(encodeOutgoingMessage(messageType = FS_ASK_PLAY_COUNT))
  }

  def sendPlayerCards(cards: List[Card]) = {
    val cardsTransformed = cards.map(
      c =>
        BasRequestResponse
          .PlayingCard()
          .withCardType(c.cardType.name)
          .withCardNumber(c.cardNumber.name))
    connection ! Tcp.Write(
      encodeOutgoingMessage(messageType = FS_SEND_PLAYER_CARDS, userCards = cardsTransformed))

  }

  def sendAllPlayerInfos(directionNameMap: mutable.Map[RelativeDirection, String]) = {
    log.debug(s"send all player infos for $name")
    connection ! Tcp.Write(
      encodeOutgoingMessage(messageType = FS_SEND_ALL_USERS_INFOS,
                            leftUserName = directionNameMap.getOrElse(LeftDirection, ""),
                            rightUserName = directionNameMap.getOrElse(RightDirection, ""),
                            crossUserName = directionNameMap.getOrElse(CrossDirection, "")))
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
      userDirection: RelativeDirection = SelfDirection,
      leftUserName: String = "",
      rightUserName: String = "",
      crossUserName: String = "",
      userCards: Seq[BasRequestResponse.PlayingCard] = List()): ByteString = {
    val outgoing = BasRequestResponse()
      .withRequestType(messageType)
      .withTextMessage(textMessage)
      .withUserDirection(UserDirection.fromValue(userDirection.code))
      .withLeftUserName(leftUserName)
      .withRightUserName(rightUserName)
      .withCrossUserName(crossUserName)
      .withUserCards(userCards)
    ByteString.fromArray(outgoing.toByteArray)
  }

  case class SendMessage(messageText: String)
  case class SetName(name: String)
  case object AskName
  case class SendPlayerInfo(name: String, relativeDirection: RelativeDirection)
  case class SendAllPlayerInfos(nameMap: mutable.Map[RelativeDirection, String])
  case class SendPlayerCards(cards: List[Card])
  case object AskPlayCount
  case object AskTrump

}
