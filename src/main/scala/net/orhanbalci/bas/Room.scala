package net.orhanbalci.bas
import akka.actor.{Actor, Props, ActorRef, ActorLogging}
import scala.collection.mutable

class Room extends Actor with ActorLogging {
  val tables            = mutable.Map[String, ActorRef]()
  val tablePlayerCounts = mutable.Map[ActorRef, Int]()

  override def receive = {
    case Room.Register(remote, connection) =>
      getSuitableTable(remote) ! Table.Register(remote, connection)
    case Room.UpdatePlayerCount(count) =>
      updatePlayerCount(sender, count)

  }

  def getSuitableTable(remote: String): ActorRef = {
    val suitableTables = tablePlayerCounts.filter(_._2 < 4)
    if (!suitableTables.isEmpty)
      suitableTables.head._1
    else {
      val table = context.actorOf(Table.props)
      tables += (remote -> table)
      table
    }
  }

  def updatePlayerCount(table: ActorRef, count: Int) = {
    tablePlayerCounts += (table -> count)
  }

}

object Room {
  def props(): Props = {
    Props(new Room)
  }
  case class UpdatePlayerCount(playerCount: Int)
  case class Register(remote: String, connection: ActorRef)
}
