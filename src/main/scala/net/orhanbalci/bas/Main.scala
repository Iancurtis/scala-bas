package net.orhanbalci.bas
import akka.actor.{ActorSystem, Props}
import java.net.InetSocketAddress

object Main extends App {

  lazy val address = {
    new InetSocketAddress("127.0.0.1", 4242)
  }
  override def main(args: Array[String]) = {
    runServer()
  }

  def runServer() = {
    val basActorSystem = ActorSystem("bas-server")
    basActorSystem.actorOf(BasServer.props(address), "server")
  }

}
