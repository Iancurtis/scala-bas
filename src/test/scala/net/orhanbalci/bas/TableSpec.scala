package net.orhanbalci.bascompile

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}
import net.orhanbalci.bas.Table

class TableSpec extends TestKit(ActorSystem("table-test-system")) with WordSpecLike with MustMatchers{
  "A Table actor " must {
    val actorRef = TestActorRef[Table]
    val connection = TestActorRef[Table]
    "register a player " in {
      actorRef ! Table.Register("my player", connection)
      actorRef.underlyingActor.players.size must equal(1)
    }
  }
}