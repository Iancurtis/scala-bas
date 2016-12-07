package net.orhanbalci.bas
import net.orhanbalci.bas.protocol._
import akka.actor.{Actor, ActorRef, Props, ActorLogging}
import akka.io.{Tcp}
import akka.util.ByteString

class Player(id: String, connection: ActorRef) extends Actor with ActorLogging{

  def receive = {
    case Tcp.Received(data) =>
      //log.debug(s"$id says ${data.utf8String}")
      val request = Player.decodeIncommingMessage(data)
      log.debug(s"$id request type ${request.requestType} request message ${request.textMessage}")
      connection ! Tcp.Write(ByteString.fromString("hello from bas"))
    case Tcp.PeerClosed =>
      context.parent ! Table.UnRegister(id)
      context stop self

  }
}

object Player {
  def props(id: String, connection: ActorRef): Props = {
    Props(new Player(id, connection)) 
  }

  def decodeIncommingMessage(data : ByteString) : BasRequestResponse ={
  	val dataArray = data.toArray
  	BasRequestResponse.parseFrom(dataArray)
  }

}
