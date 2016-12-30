package net.orhanbalci.bas

import akka.actor.{ActorSystem, Props, ActorRef, Actor, ActorRefFactory}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.{MustMatchers, WordSpecLike, BeforeAndAfterEach}
import akka.io.Tcp
import com.typesafe.config.ConfigFactory
import net.orhanbalci.bas.protocol.{
  CardType => ProCardType,
  CardNumber => ProCardNumber,
  BasRequestResponse,
  RequestResponseType,
  PlayingCard
}

import scala.concurrent.duration._

class TableSpec
    extends TestKit(
      ActorSystem(
        "table-test-system",
        ConfigFactory.parseString("""
    akka.loggers = ["akka.testkit.TestEventListener"]
    akka.stdout-loglevel = "OFF"
    akka.loglevel = "OFF"
    """)
      ))
    with WordSpecLike
    with MustMatchers
    with ImplicitSender
    with BeforeAndAfterEach {

  var testPlayer = TestProbe()
  val playerFactory = (_: ActorRefFactory, _: String, _: ActorRef) => {
    testPlayer.ref
  }
  var testParent         = TestProbe()
  var firstTable         = TestActorRef(new Table(playerFactory))
  var secondTable        = testParent.childActorOf(Props(classOf[Table], playerFactory))
  var connection         = TestActorRef(new Table(playerFactory))
  var tcpMessageReceiver = TestProbe()

  override def beforeEach() = {
    testPlayer = TestProbe()
    testParent = TestProbe()
    firstTable = TestActorRef(new Table(playerFactory))
    secondTable = testParent.childActorOf(Props(classOf[Table], playerFactory))
    connection = TestActorRef(new Table(playerFactory))
    tcpMessageReceiver = TestProbe()
  }

  "A Table actor " must {
    "register a player " in {
      firstTable ! Table.Register("first player", connection)
      firstTable.underlyingActor.players.size must equal(1)
    }

    "unregister player " in {
      firstTable ! Table.Register("first player", connection)
      firstTable ! Table.UnRegister("first player")
      firstTable.underlyingActor.players.size must equal(0)
    }

    "register player with name " in {
      val playerName = "second player"
      firstTable ! Table.Register(playerName, connection)
      firstTable.underlyingActor.players.keySet.contains(playerName) must equal(true)
    }

    "update player count of table when registering " in {
      secondTable ! Table.Register("test player", connection)
      testParent.expectMsg(Room.UpdatePlayerCount(1))
    }

    "send Tcp.Register message to given actor" in {
      val playerName = "second player"
      firstTable ! Table.Register(playerName, tcpMessageReceiver.ref)
      tcpMessageReceiver.expectMsgPF() {
        case Tcp.Register(_, false, true) => ()
      }
    }

    "seat players" in {
      firstTable.underlyingActor.seatPlayer(TestProbe().ref)
      firstTable.underlyingActor.playerSeats.size must equal(1)
    }

    "deal cards when 4 players seated" in {
      val firstPlayer = TestProbe()
      firstTable.underlyingActor.seatPlayer(firstPlayer.ref)
      firstTable.underlyingActor.seatPlayer(TestProbe().ref)
      firstTable.underlyingActor.seatPlayer(TestProbe().ref)
      firstTable.underlyingActor.seatPlayer(TestProbe().ref)
      firstTable.underlyingActor.dealCards must equal(true)
      firstTable.underlyingActor.playerCards(firstPlayer.ref).size must equal(13)
    }

    "not deal cards when less than 4 players seated" in {
      val firstPlayer = TestProbe()
      firstTable.underlyingActor.seatPlayer(firstPlayer.ref)
      firstTable.underlyingActor.seatPlayer(TestProbe().ref)
      firstTable.underlyingActor.seatPlayer(TestProbe().ref)
      firstTable.underlyingActor.dealCards must equal(false)
    }

    "deal unique cards when 4 players seated" in {
      val firstPlayer  = TestProbe()
      val secondPlayer = TestProbe()
      val thirdPlayer  = TestProbe()
      val fourthPlayer = TestProbe()
      firstTable.underlyingActor.seatPlayer(firstPlayer.ref)
      firstTable.underlyingActor.seatPlayer(secondPlayer.ref)
      firstTable.underlyingActor.seatPlayer(thirdPlayer.ref)
      firstTable.underlyingActor.seatPlayer(fourthPlayer.ref)
      firstTable.underlyingActor.dealCards must equal(true)
      firstTable.underlyingActor
        .playerCards(firstPlayer.ref)
        .intersect(firstTable.underlyingActor.playerCards(secondPlayer.ref))
        .isEmpty must equal(true)
      firstTable.underlyingActor
        .playerCards(firstPlayer.ref)
        .intersect(firstTable.underlyingActor.playerCards(thirdPlayer.ref))
        .isEmpty must equal(true)
      firstTable.underlyingActor
        .playerCards(firstPlayer.ref)
        .intersect(firstTable.underlyingActor.playerCards(fourthPlayer.ref))
        .isEmpty must equal(true)
      firstTable.underlyingActor
        .playerCards(secondPlayer.ref)
        .intersect(firstTable.underlyingActor.playerCards(thirdPlayer.ref))
        .isEmpty must equal(true)
      firstTable.underlyingActor
        .playerCards(secondPlayer.ref)
        .intersect(firstTable.underlyingActor.playerCards(fourthPlayer.ref))
        .isEmpty must equal(true)
      firstTable.underlyingActor
        .playerCards(thirdPlayer.ref)
        .intersect(firstTable.underlyingActor.playerCards(fourthPlayer.ref))
        .isEmpty must equal(true)
    }

    "get next player in turn" in {
      firstTable.underlyingActor.inPlayTurn = South
      firstTable.underlyingActor.getNextInPlayTurn must equal(East)
    }

    "set players name" in {
      val player = TestProbe();
      firstTable.underlyingActor.setPlayerName(player.ref, "orhan")
      firstTable.underlyingActor.playerNames.size must equal(1)
      firstTable.underlyingActor.playerNames(player.ref) must equal("orhan")
    }

    "increment players earned games" in {
      val player = TestProbe();
      firstTable.underlyingActor.incrementEarnedCount(player.ref)
      firstTable.underlyingActor.gamesEarned.size must equal(1)
      firstTable.underlyingActor.gamesEarned(player.ref) must equal(1)
      firstTable.underlyingActor.incrementEarnedCount(player.ref)
      firstTable.underlyingActor.gamesEarned(player.ref) must equal(2)
    }

    "decide whether a card is of type trump" in {
      firstTable.underlyingActor.trump = (TestProbe().ref, AceOfDiamonds)
      firstTable.underlyingActor.isTrumpType(TwoOfDiamonds) must equal(true)
      firstTable.underlyingActor.isTrumpType(TwoOfClubs) must equal(false)
    }

    "send text messages to players" in {
      firstTable ! Table.Register("first player", connection)
      firstTable ! Table.PlayerMessage(
        BasRequestResponse(requestType = RequestResponseType.FC_SEND_TEXT_MESSAGE,
                           textMessage = "Hello Test"))
      testPlayer.expectMsgAllOf(100 millis, Player.SendMessage("Hello Test"), Player.AskName)
    }

    "respond to send name message" in {
      firstTable ! Table.Register("first player", connection)
      testPlayer.send(
        firstTable,
        Table.PlayerMessage(
          BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME, name = "orhan")))
      firstTable.underlyingActor.playerNames.size must equal(1)
      firstTable.underlyingActor.playerSeats.size must equal(1)
      firstTable.underlyingActor.playerNames(testPlayer.ref) must equal("orhan")
    }

    "be able to record play counts" in {
      val firstPlayer  = TestProbe()
      val secondPlayer = TestProbe()
      val thirdPlayer  = TestProbe()
      val fourthPlayer = TestProbe()
      firstTable ! Table.Register("first player", connection)
      firstTable ! Table.Register("second player", connection)
      firstTable ! Table.Register("third player", connection)
      firstTable ! Table.Register("fourth player", connection)
      firstPlayer.send(firstTable,
                       Table.PlayerMessage(
                         BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                            name = "first player")))
      secondPlayer.send(firstTable,
                        Table.PlayerMessage(
                          BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                             name = "second player")))
      thirdPlayer.send(firstTable,
                       Table.PlayerMessage(
                         BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                            name = "third player")))
      fourthPlayer.send(firstTable,
                        Table.PlayerMessage(
                          BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                             name = "fourth player")))

      val playerInTurn: TestProbe =
        if (firstPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          firstPlayer
        else if (secondPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          secondPlayer
        else if (thirdPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          thirdPlayer
        else if (fourthPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          fourthPlayer
        else
          firstPlayer

      playerInTurn.send(
        firstTable,
        Table.PlayerMessage(
          BasRequestResponse(requestType = RequestResponseType.FC_SEND_PLAY_COUNT, playCount = 5)))

      firstTable.underlyingActor.playCounts.size must equal(1)
      firstTable.underlyingActor.playCounts(playerInTurn.ref) must equal(5)

      val nextPlayerInTurn: TestProbe =
        if (firstPlayer.ref == firstTable.underlyingActor.playerSeats(East))
          firstPlayer
        else if (secondPlayer.ref == firstTable.underlyingActor.playerSeats(East))
          secondPlayer
        else if (thirdPlayer.ref == firstTable.underlyingActor.playerSeats(East))
          thirdPlayer
        else if (fourthPlayer.ref == firstTable.underlyingActor.playerSeats(East))
          fourthPlayer
        else
          firstPlayer

      val s = nextPlayerInTurn.fishForMessage(1 seconds) {
        case Player.SetName(_)            => false
        case Player.SendAllPlayerInfos(_) => false
        case Player.AskPlayCount(_)       => true
        case Player.SendPlayerCards(_)    => false
      }

      s.getClass must equal(classOf[Player.AskPlayCount])

    }

    "be able to register trump card " in {
      val firstPlayer  = TestProbe()
      val secondPlayer = TestProbe()
      val thirdPlayer  = TestProbe()
      val fourthPlayer = TestProbe()
      firstTable ! Table.Register("first player", connection)
      firstTable ! Table.Register("second player", connection)
      firstTable ! Table.Register("third player", connection)
      firstTable ! Table.Register("fourth player", connection)
      firstPlayer.send(firstTable,
                       Table.PlayerMessage(
                         BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                            name = "first player")))
      secondPlayer.send(firstTable,
                        Table.PlayerMessage(
                          BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                             name = "second player")))
      thirdPlayer.send(firstTable,
                       Table.PlayerMessage(
                         BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                            name = "third player")))
      fourthPlayer.send(firstTable,
                        Table.PlayerMessage(
                          BasRequestResponse(requestType = RequestResponseType.FC_SEND_NAME,
                                             name = "fourth player")))

      val playerInTurn: TestProbe =
        if (firstPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          firstPlayer
        else if (secondPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          secondPlayer
        else if (thirdPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          thirdPlayer
        else if (fourthPlayer.ref == firstTable.underlyingActor.playerSeats(South))
          fourthPlayer
        else
          firstPlayer

      playerInTurn.send(
        firstTable,
        Table.PlayerMessage(
          BasRequestResponse(requestType = RequestResponseType.FC_SEND_TRUMP,
                             cardInPlay = Some(
                               PlayingCard()
                                 .withCardType(ProCardType.CT_SPADES)
                                 .withCardNumber(ProCardNumber.CN_ACE))))
      )
      firstTable.underlyingActor.trump._1 must equal(playerInTurn.ref)
      firstTable.underlyingActor.trump._2 must equal(AceOfSpades)

    }
  }
}
