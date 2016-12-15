package net.orhanbalci.bascompile

import akka.actor.{ActorSystem, Props, ActorRef, Actor}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.{MustMatchers, WordSpecLike}
import net.orhanbalci.bas.Table
import net.orhanbalci.bas.Room
import akka.io.Tcp

class TableSpec extends TestKit(ActorSystem("table-test-system")) with WordSpecLike with MustMatchers with ImplicitSender{
  "A Table actor " must {
    val testParent = TestProbe()
    val firstTable = TestActorRef[Table]
    val secondTable = testParent.childActorOf(Props[Table])
    val connection = TestActorRef[Table]
    val tcpMessageReceiver = TestProbe()
    "register a player " in {
      firstTable ! Table.Register("first player", connection)
      firstTable.underlyingActor.players.size must equal(1)
    }

    "register player with name " in {
      val playerName  = "second player"
      firstTable ! Table.Register(playerName, connection)
      firstTable.underlyingActor.players.size must equal(2) 
      firstTable.underlyingActor.players.keySet.contains(playerName) must equal(true)
    }

    "update player count of table when registering " in {
      secondTable ! Table.Register("test player", connection)
      testParent.expectMsg(Room.UpdatePlayerCount(1))
    }

    "send Tcp.Register message to given actor" in {
      val playerName  = "second player"
      firstTable ! Table.Register(playerName, tcpMessageReceiver.ref)
      tcpMessageReceiver.expectMsgPF() {
        case Tcp.Register(_, false, true) => ()
      }

    }
  }
}