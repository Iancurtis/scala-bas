package net.orhanbalci.bascompile

import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit, TestProbe}
import org.scalatest.{MustMatchers, WordSpecLike}
import net.orhanbalci.bas.Table
import net.orhanbalci.bas.Room

class TableSpec extends TestKit(ActorSystem("table-test-system")) with WordSpecLike with MustMatchers with ImplicitSender{
  "A Table actor " must {
    val testParent = TestProbe()
    val firstTable = TestActorRef[Table]
    val secondTable = testParent.childActorOf(Props[Table])
    val connection = TestActorRef[Table]
    "register a player " in {
      firstTable ! Table.Register("my player", connection)
      firstTable.underlyingActor.players.size must equal(1)
    }

    "update player count of table when registering " in {
      secondTable ! Table.Register("test player", connection)
      testParent.expectMsg(Room.UpdatePlayerCount(1))
    }
  }
}