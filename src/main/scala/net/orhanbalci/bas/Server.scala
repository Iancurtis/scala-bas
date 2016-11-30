package net.orhanbalci.bas

import akka.actor.{Actor, Props}
import akka.io.{Tcp, IO}
import java.net.InetSocketAddress
import akka.event.Logging

object BasServer {
  def props(host: InetSocketAddress): Props = {
    Props(new BasServer(host))
  }
}

class BasServer(host: InetSocketAddress) extends Actor {
  import context.system

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

  }
}
