package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import net.orhanbalci.bas.protocol.BasRequestResponse.RequestResponseType._
import akka.actor.{Actor, Props, ActorRef, ActorLogging}
import akka.io.Tcp
import scala.collection.mutable
import scala.util.Random

class Table extends Actor with ActorLogging {
  var players          = mutable.Map[String, ActorRef]()
  var playerNames      = mutable.Map[ActorRef, String]()
  var playerSeats      = mutable.Map[Seat, ActorRef]()
  var playerCards      = mutable.Map[ActorRef, List[Card]]()
  var playCounts       = Map[ActorRef, Integer]()
  var whosTurn: Seat   = South
  var moveCount        = 13
  var gameCount        = 11
  var inPlayTurn: Seat = South

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
        //sendNewPlayerInfo(senderPlayer)
        sendAllPlayerInfos
        if (dealCards) {
          sendPlayerCards
          askPlayCount(South)
        }
      case FC_SEND_PLAY_COUNT =>
        setPlayCount(senderPlayer, playerRequest.playCount)
        if (playCounts.size < 4)
          askPlayCount(getNextInPlayTurn)
        else
          askTrump
    }
  }

  def askTrump = {
    playCounts.maxBy(_._2)._1 ! Player.AskTrump
  }

  def setPlayCount(player: ActorRef, playCount: Integer) = {
    playCounts = playCounts + (player -> playCount)
  }

  def askPlayCount(seat: Seat) = {
    playerSeats(seat) ! Player.AskPlayCount
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
}

object Table {
  def props(): Props = {
    Props(new Table())
  }

  case class Register(remote: String, connection: ActorRef)
  case class UnRegister(playerId: String)
  case class PlayerMessage(playerRequest: BasRequestResponse)
}
