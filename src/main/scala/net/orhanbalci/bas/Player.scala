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
    case AskPlayCount(playCounts) =>
      sendAskPlayCount(playCounts)
    case AskTrump =>
      askTrump
    case SendWhosTurn(relativeDirection) =>
      sendWhosTurn(relativeDirection)
  }

  def sendWhosTurn(relativeDirection: RelativeDirection) = {}

  def askTrump = {
    connection ! Tcp.Write(encodeOutgoingMessage(messageType = FS_ASK_TRUMP))
  }

  def sendAskPlayCount(playCounts: Map[RelativeDirection, Integer]) = {
    connection ! Tcp.Write(
      encodeOutgoingMessage(messageType = FS_ASK_PLAY_COUNT,
                            leftPlayCount = playCounts.getOrElse(LeftDirection, 0),
                            rightPlayCount = playCounts.getOrElse(RightDirection, 0),
                            crossPlayCount = playCounts.getOrElse(CrossDirection, 0)))
  }

  def sendPlayerCards(cards: List[Card]) = {
    val cardsTransformed = cards.map(
      c =>
        BasRequestResponse
          .PlayingCard()
          .withCardType(convertCardType(c))
          .withCardNumber(convertCardNumber(c)))
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

  def encodeOutgoingMessage(messageType: RequestResponseType = Unrecognized(-1),
                            textMessage: String = "",
                            userDirection: RelativeDirection = SelfDirection,
                            leftUserName: String = "",
                            rightUserName: String = "",
                            crossUserName: String = "",
                            userCards: Seq[BasRequestResponse.PlayingCard] = List(),
                            leftPlayCount: Integer = 0,
                            rightPlayCount: Integer = 0,
                            crossPlayCount: Integer = 0): ByteString = {
    val outgoing = BasRequestResponse()
      .withRequestType(messageType)
      .withTextMessage(textMessage)
      .withUserDirection(UserDirection.fromValue(userDirection.code))
      .withLeftUserName(leftUserName)
      .withRightUserName(rightUserName)
      .withCrossUserName(crossUserName)
      .withUserCards(userCards)
      .withLeftPlayCount(leftPlayCount)
      .withRightPlayCount(rightPlayCount)
      .withCrossPlayCount(crossPlayCount)
    ByteString.fromArray(outgoing.toByteArray)
  }

  def convertCardType(card : Card ) : BasRequestResponse.CardType = {
    card.cardType match {
      case Spades => BasRequestResponse.CardType.CT_SPADES
      case Clubs => BasRequestResponse.CardType.CT_CLUBS
      case Diamonds => BasRequestResponse.CardType.CT_DIAMONDS
      case Hearts => BasRequestResponse.CardType.CT_HEARTS
    }
  }

  def convertCardNumber(card : Card) : BasRequestResponse.CardNumber = {
    card.cardNumber match {
      case Ace =>  BasRequestResponse.CardNumber.CN_ACE
      case King => BasRequestResponse.CardNumber.CN_KING
      case Queen => BasRequestResponse.CardNumber.CN_QUEEN
      case Jack => BasRequestResponse.CardNumber.CN_JACK
      case Ten => BasRequestResponse.CardNumber.CN_TEN
      case Nine => BasRequestResponse.CardNumber.CN_NINE
      case Eight => BasRequestResponse.CardNumber.CN_EIGHT
      case Seven => BasRequestResponse.CardNumber.CN_SEVEN
      case Six => BasRequestResponse.CardNumber.CN_SIX
      case Five => BasRequestResponse.CardNumber.CN_FIVE
      case Four => BasRequestResponse.CardNumber.CN_FOUR
      case Three => BasRequestResponse.CardNumber.CN_THREE
      case Two => BasRequestResponse.CardNumber.CN_TWO
    }
  }

  case class SendMessage(messageText: String)
  case class SetName(name: String)
  case object AskName
  case class SendPlayerInfo(name: String, relativeDirection: RelativeDirection)
  case class SendAllPlayerInfos(nameMap: mutable.Map[RelativeDirection, String])
  case class SendPlayerCards(cards: List[Card])
  case class AskPlayCount(playCounts: Map[RelativeDirection, Integer])
  case object AskTrump
  case class SendWhosTurn(relativeDirection: RelativeDirection)

}
