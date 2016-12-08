package net.orhanbalci.bas

sealed abstract class Seat(name:String) {
	def getLeft : Seat
	def getRight : Seat
	def getCross : Seat
}

case object North extends Seat("North") {
	def getLeft = East
	def getRight = West
	def getCross = South
}

case object South extends Seat("South"){
	def getLeft  = West
	def getRight = East
	def getCross = North
}

case object West extends Seat("West"){
	def getLeft = North
	def getRight = South
	def getCross = East
}

case object East extends Seat("East"){
	def getLeft = South
	def getRight = North
	def getCross = West

}

object Seats {
	val values = List(North,South,East,West)
}