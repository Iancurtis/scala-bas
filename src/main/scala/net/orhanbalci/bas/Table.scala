package net.orhanbalci.bas
import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp
import net.orhanbalci.bas.protocol.RequestResponseType._
import net.orhanbalci.bas.protocol.CardType._
import net.orhanbalci.bas.protocol.CardNumber._
import net.orhanbalci.bas.protocol.{
  CardType => ProCardType,
  CardNumber => ProCardNumber,
  BasRequestResponse,
  RequestResponseType,
  PlayingCard
}

import scala.collection.mutable
import scala.util.Random

class Table extends Actor with ActorLogging {
  var players                 = mutable.Map[String, ActorRef]()
  var playerNames             = mutable.Map[ActorRef, String]()
  var playerSeats             = mutable.Map[Seat, ActorRef]()
  var playerCards             = mutable.Map[ActorRef, List[Card]]()
  var playCounts              = Map[ActorRef, Integer]()
  var whosTurn: Seat          = South
  var moveCount               = 13
  var gameCount               = 11
  var inPlayTurn: Seat        = South
  var trump: (ActorRef, Card) = (self, AceOfDiamonds)
  var cardsOnTable            = List[(Seat, Card)]()

  override def receive = {
    case Table.Register(remote, connection) =>
      val playerActor = context.actorOf(Player.props(remote, connection))
      players += (remote -> playerActor)
      connection ! Tcp.Register(playerActor)
      context.parent ! Room.UpdatePlayerCount(players.size)
      playerActor ! Player.AskName
      log.info(s"Player registered $remote -> $playerActor")
    case Table.UnRegister(playerId) =>
      playerNames -= players(playerId)
      playerSeats = playerSeats.filterNot(seatRef => seatRef._2 == players(playerId))
      playerCards -= players(playerId)
      playCounts = playCounts - players(playerId)
      players -= playerId
      context.parent ! Room.UpdatePlayerCount(players.size)
      log.info(s"Unregistered $playerId")
    case Table.PlayerMessage(playerRequest) =>
      handlePlayerMessage(playerRequest, sender)
  }

  def handlePlayerMessage(playerRequest: BasRequestResponse, senderPlayer: ActorRef) = {
    playerRequest.requestType match {
      case FC_SEND_TEXT_MESSAGE =>
        sendUserMessage(senderPlayer, playerRequest.textMessage)
      case FC_SEND_NAME =>
        setPlayerName(senderPlayer, playerRequest.name)
        seatPlayer(senderPlayer)
        sendAllPlayerInfos
        if (dealCards) {
          sendPlayerCards
          askPlayCount(inPlayTurn)
        }
      case FC_SEND_PLAY_COUNT =>
        if (playerSeats(inPlayTurn) == senderPlayer) {
          setPlayCount(senderPlayer, playerRequest.playCount)
          if (playCounts.size < 4)
            askPlayCount(getNextInPlayTurn)
          else
            askTrump
        } else {
          log.debug("Not your turn " + inPlayTurn)
        }
      case FC_SEND_TRUMP =>
        if (playerSeats(inPlayTurn) == senderPlayer) {
          setTrump(senderPlayer,
                   playerRequest.cardInPlay.getOrElse(PlayingCard(CT_SPADES, CN_ACE)).cardType)
          sendWhosTurn(inPlayTurn)
          sendTrump(trump._2)
        }
      case FC_PLAY_CARD =>
        if (playerSeats(inPlayTurn) == senderPlayer) {
          if (isOktoPlayCard(convertCard(playerRequest.cardInPlay.get), senderPlayer)) {
            playerCards(senderPlayer) =
              playerCards(senderPlayer).filterNot(_ == convertCard(playerRequest.cardInPlay.get))
            cardsOnTable = (getPlayerSeat(senderPlayer), convertCard(playerRequest.cardInPlay.get)) :: cardsOnTable
            sendWhosTurn(getNextInPlayTurn)
          }
        }
    }
  }

  def isOktoPlayCard(card: Card, player: ActorRef): Boolean = {
    if (validateSameCardType(card)) { //ilk atilan kart oynaniyor
      if (hasBiggerCard(cardsOnTable(0)._2.cardType, player)) { // masadaki kartlarin en buyugunden buyuk kagidi var mi
        if (isTrumpOnTable) // koz oynanmis mi
          true
        else {
          if (isCardBiggerThanOthers(cardsOnTable(0)._2.cardType, card))
            true
          else
            false
        }
      } else
        true
    } else if (isTrumpType(card)) { // koz oynaniyor
      if (hasCardType(cardsOnTable(0)._2.cardType, player)) // ilk oynanan kartc cinsinden elinde var mi
        false
      else if (hasBiggerCard(trump._2.cardType, player)) { // el yukseltebilir mi
        if (isCardBiggerThanOthers(trump._2.cardType, card)) // el yukseltmis mi
          true
        else
          false
      } else
        true
    } else {
      if (hasCardType(cardsOnTable(0)._2.cardType, player)) // ilk oynanan karttan elinde var mi
        false
      else if (hasCardType(trump._2.cardType, player)) // elinde koz varmi
        false
      else
        true
    }
  }

  def isTrumpType(card: Card): Boolean = {
    card.cardType == trump._2.cardType
  }

  def validateSameCardType(card: Card): Boolean = {
    if (!cardsOnTable.isEmpty) {
      card.cardType == cardsOnTable(0)._2.cardType
    } else
      true
  }

  def hasCardType(ct: CardType, player: ActorRef): Boolean = {
    playerCards(player).exists(c => c.cardType == ct)
  }

  def hasBiggerCard(ct: CardType, player: ActorRef): Boolean = {
    if (cardsOnTable.size > 0) {
      val sameCardTypes   = cardsOnTable.filter(crd => crd._2.cardType == ct)
      val sortedCardTypes = sameCardTypes.sortWith(_._2.cardNumber < _._2.cardNumber)
      playerCards(player).exists(_.cardNumber > sortedCardTypes(0)._2.cardNumber)
    } else
      true
  }

  def isCardBiggerThanOthers(ct: CardType, card: Card): Boolean = {
    if (cardsOnTable.size > 0) {
      val sameCardTypes   = cardsOnTable.filter(crd => crd._2.cardType == ct)
      val sortedCardTypes = sameCardTypes.sortWith(_._2.cardNumber < _._2.cardNumber)
      card.cardNumber > sortedCardTypes(0)._2.cardNumber
    } else
      true
  }

  def isTrumpOnTable(): Boolean = {
    cardsOnTable.exists(card => card._2.cardType == trump._2.cardType)
  }

  def convertCard(card: PlayingCard): Card = {
    val cardType = card.cardType match {
      case CT_SPADES   => Spades
      case CT_CLUBS    => Clubs
      case CT_DIAMONDS => Diamonds
      case CT_HEARTS   => Hearts
      case _           => log.error("Undefined card type in Table::convertCard"); Spades
    }

    val cardNumber = card.cardNumber match {
      case CN_ACE   => Ace
      case CN_KING  => King
      case CN_QUEEN => Queen
      case CN_JACK  => Jack
      case CN_TEN   => Ten
      case CN_NINE  => Nine
      case CN_EIGHT => Eight
      case CN_SEVEN => Seven
      case CN_SIX   => Six
      case CN_FIVE  => Five
      case CN_FOUR  => Four
      case CN_THREE => Three
      case CN_TWO   => Two
      case _        => log.error("Undefined card number in Table::convertCard"); Ace
    }

    new Card(cardType, cardNumber)
  }

  def sendTrump(card: Card) = {
    players foreach {
      case (_, actorRef) => actorRef ! Player.SendTrump(card)
    }
  }

  def sendWhosTurn(playerInTurnSeat: Seat) = {
    playerSeats foreach {
      case (seat, player) =>
        player ! Player.SendWhosTurn(seat.getDirectionRelative(playerInTurnSeat),
                                     playerCards(playerSeats(seat)),
                                     cardsOnTable)
    }
  }

  def setTrump(senderPlayer: ActorRef, cardType: ProCardType) = {
    trump = cardType match {
      case CT_SPADES   => (senderPlayer, AceOfSpades)
      case CT_CLUBS    => (senderPlayer, AceOfClubs)
      case CT_DIAMONDS => (senderPlayer, AceOfDiamonds)
      case CT_HEARTS   => (senderPlayer, AceOfHearts)
    }

    inPlayTurn = playerSeats.find(_._2 == senderPlayer) match {
      case Some(playerSeat) => playerSeat._1
      case None             => South
    }
  }

  def askTrump = {
    playCounts.maxBy(_._2)._1 ! Player.AskTrump
  }

  def setPlayCount(player: ActorRef, playCount: Integer) = {
    playCounts = playCounts + (player -> playCount)
  }

  def askPlayCount(seat: Seat) = {
    val playCountMap: Map[RelativeDirection, Integer] = playCounts.flatMap {
      case (playerActor, playCount) =>
        playerSeats.find(playerSeat => playerSeat._2 == playerActor) match {
          case Some(innerSeat) => Some(seat.getDirectionRelative(innerSeat._1) -> playCount)
          case None            => log.debug("askPlayCount can not find seat for player "); None
        }
    }
    log.debug(s"playcounts size ${playCounts.size}")
    playerSeats(seat) ! Player.AskPlayCount(playCountMap)
  }

  def sendAllPlayerInfos = {
    playerSeats.foreach {
      case (seat, playerActor) => {
        val nameMap = mutable.Map[RelativeDirection, String]()
        playerSeats.foreach {
          case (innerSeat, innerPlayerActor) =>
            if (seat != innerSeat) {
              val relativeDirection = seat.getDirectionRelative(innerSeat)
              val name              = playerNames(innerPlayerActor)
              nameMap += (relativeDirection -> name)
            }
        }
        playerActor ! Player.SendAllPlayerInfos(nameMap)
      }
    }
  }

  def getNextInPlayTurn: Seat = {
    inPlayTurn = inPlayTurn.getRight
    inPlayTurn
  }

  def sendPlayerCards = {
    log.debug("sending player cards")
    playerCards.foreach {
      case (player, cards) => player ! Player.SendPlayerCards(cards)
    }
  }

  def sendUserMessage(sender: ActorRef, message: String) = {
    players.foreach(player => if (player._2 != sender) { player._2 ! Player.SendMessage(message) })
  }

  def setPlayerName(player: ActorRef, name: String) = {
    player ! Player.SetName(name)
    playerNames += (player -> name)
  }

  def seatPlayer(player: ActorRef) = {
    getEmptySeat match {
      case Some(seat) => {
        playerSeats += (seat -> player); log.info(s"Player seated $seat $player")
      }
      case None => log.info("Oturacak yer yok")
    }
  }

  def getEmptySeat: Option[Seat] = {
    val difference = Seats.values filterNot playerSeats.keySet
    difference.headOption
  }

  def sendNewPlayerInfo(senderPlayer: ActorRef) {
    val newPlayerSeat = playerSeats.find(_._2 == senderPlayer).get._1
    val name          = playerNames(senderPlayer)
    players.foreach(player =>
      if (player._2 != senderPlayer) {
        val seat = playerSeats.find(_._2 == player._2).get._1
        player._2 ! Player.SendPlayerInfo(name, seat.getDirectionRelative(newPlayerSeat))
    })
  }

  def dealCards: Boolean = {
    if (playerSeats.size == 4) {
      var deck = Deck.values.toList
      deck = Random.shuffle(deck)
      val cardGroups = deck.sliding(13).toList
      playerCards += (playerSeats(North) -> cardGroups(0))
      playerCards += (playerSeats(South) -> cardGroups(1))
      playerCards += (playerSeats(East)  -> cardGroups(2))
      playerCards += (playerSeats(West)  -> cardGroups(3))
      true
    } else {
      log.debug(s"player seats size ${playerSeats.size}")
      false
    }
  }

  def getPlayerSeat(player: ActorRef): Seat = {
    playerSeats.find(_._2 == player).get._1
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
