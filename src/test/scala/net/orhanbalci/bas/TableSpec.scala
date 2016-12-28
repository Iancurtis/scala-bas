package net.orhanbalci.bas

import akka.actor.{ActorSystem, Props, ActorRef, Actor}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.{MustMatchers, WordSpecLike, BeforeAndAfterEach}
import net.orhanbalci.bas.Table
import net.orhanbalci.bas.Room
import akka.io.Tcp

class TableSpec
    extends TestKit(ActorSystem("table-test-system"))
    with WordSpecLike
    with MustMatchers
    with ImplicitSender
    with BeforeAndAfterEach {

  var testParent         = TestProbe()
  var firstTable         = TestActorRef[Table]
  var secondTable        = testParent.childActorOf(Props[Table])
  var connection         = TestActorRef[Table]
  var tcpMessageReceiver = TestProbe()

  override def beforeEach() = {
    testParent = TestProbe()
    firstTable = TestActorRef[Table]
    secondTable = testParent.childActorOf(Props[Table])
    connection = TestActorRef[Table]
    tcpMessageReceiver = TestProbe()
  }

  "A Table actor " must {
    "register a player " in {
      firstTable ! Table.Register("first player", connection)
      firstTable.underlyingActor.players.size must equal(1)
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
  }
}
