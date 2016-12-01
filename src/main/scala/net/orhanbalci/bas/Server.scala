package net.orhanbalci.bas

import akka.actor.{Actor, Props}
import akka.io.{Tcp, IO}
import java.net.InetSocketAddress
import akka.event.Logging

class BasServer(host: InetSocketAddress) extends Actor {
  import context.system

  val room = context.actorOf(Room.props)
  var log = Logging(context.system, this)

  IO(Tcp) ! Tcp.Bind(self, host)
  def receive = {
    case Tcp.Bound(localAddress) =>
      println(s"Spawned $localAddress")
      log.debug(s"Spawned $localAddress")
    case Tcp.CommandFailed(_: Tcp.Bind) =>
      println("Bind failed stopping server")
      context stop self
    case Tcp.Connected(remote, local) =>
      log.debug(s"Connected $remote")
      room ! Room.Register(remote.toString, sender)

  }
}

object BasServer {
  def props(host: InetSocketAddress): Props = {
    Props(new BasServer(host))
  }
}
