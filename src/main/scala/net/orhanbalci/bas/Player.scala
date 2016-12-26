package net.orhanbalci.bas
import net.orhanbalci.bas.protocol.{
  BasRequestResponse,
  PlayingCard,
  CardNumber => ProCardNumber,
  CardType => ProCardType,
  RequestResponseType,
  UserDirection
}
import net.orhanbalci.bas.protocol.RequestResponseType._
import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import akka.io.Tcp
import akka.util.ByteString

import scala.collection.mutable

class Player(id: String, connection: ActorRef) extends Actor with ActorLogging {

  import Player._
  var name = ""

  context.watch(connection)
  def receive = {
    case Terminated(connection) =>
      context.parent ! Table.UnRegister(id)
      context stop self
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
    case SendWhosTurn(relativeDirection, userCards, tableCards) =>
      sendWhosTurn(relativeDirection, userCards, tableCards)
    case SendTrump(trumpCard) =>
      sendTrump(trumpCard)
  }

  def sendTrump(card: Card) = {
    connection ! Tcp.Write(
      encodeOutgoingMessage(messageType = FS_SEND_TRUMP,
                            cardInPlay = PlayingCard()
                              .withCardType(convertCardType(card))
                              .withCardNumber(convertCardNumber(card))))
  }

  def sendWhosTurn(relativeDirection: RelativeDirection,
                   userCards: List[Card],
                   tableCards: List[(Seat, Card)]) = {
    val userCardsTransformed = userCards.map(c =>
      PlayingCard().withCardType(convertCardType(c)).withCardNumber(convertCardNumber(c)))
    val tableCardsTransformed = tableCards.map(c =>
      PlayingCard().withCardType(convertCardType(c._2)).withCardNumber(convertCardNumber(c._2)))
    connection ! Tcp.Write(
      encodeOutgoingMessage(messageType = FS_SEND_WHOS_TURN,
                            userDirection = relativeDirection,
                            userCards = userCardsTransformed,
                            tableCards = tableCardsTransformed))
  }

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
    val cardsTransformed = cards.map(c =>
      PlayingCard().withCardType(convertCardType(c)).withCardNumber(convertCardNumber(c)))
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
      userCards: Seq[PlayingCard] = List(),
      leftPlayCount: Integer = 0,
      rightPlayCount: Integer = 0,
      crossPlayCount: Integer = 0,
      cardInPlay: PlayingCard =
        PlayingCard(ProCardType.Unrecognized(0), ProCardNumber.Unrecognized(0)),
      tableCards: Seq[PlayingCard] = List()): ByteString = {
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
      .withCardInPlay(cardInPlay)
      .withCardsOnTable(tableCards)
    ByteString.fromArray(outgoing.toByteArray)
  }

  def convertCardType(card: Card): ProCardType = {
    card.cardType match {
      case Spades   => ProCardType.CT_SPADES
      case Clubs    => ProCardType.CT_CLUBS
      case Diamonds => ProCardType.CT_DIAMONDS
      case Hearts   => ProCardType.CT_HEARTS
    }
  }

  def convertCardNumber(card: Card): ProCardNumber = {
    card.cardNumber match {
      case Ace   => ProCardNumber.CN_ACE
      case King  => ProCardNumber.CN_KING
      case Queen => ProCardNumber.CN_QUEEN
      case Jack  => ProCardNumber.CN_JACK
      case Ten   => ProCardNumber.CN_TEN
      case Nine  => ProCardNumber.CN_NINE
      case Eight => ProCardNumber.CN_EIGHT
      case Seven => ProCardNumber.CN_SEVEN
      case Six   => ProCardNumber.CN_SIX
      case Five  => ProCardNumber.CN_FIVE
      case Four  => ProCardNumber.CN_FOUR
      case Three => ProCardNumber.CN_THREE
      case Two   => ProCardNumber.CN_TWO
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
  case class SendTrump(card: Card)
  case class SendWhosTurn(relativeDirection: RelativeDirection,
                          userCards: List[Card],
                          tableCards: List[(Seat, Card)])

}
