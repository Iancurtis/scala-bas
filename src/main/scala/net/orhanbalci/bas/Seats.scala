package net.orhanbalci.bas

sealed abstract class RelativeDirection(code : Int)
case object CrossDirection extends RelativeDirection(0)
case object LeftDirection extends RelativeDirection(1)
case object RightDirection extends RelativeDirection(2)


sealed abstract class Seat(name:String) {
	def getLeft : Seat
	def getRight : Seat
	def getCross : Seat
	def getDirectionRelative(seat : Seat) : RelativeDirection
}

case object North extends Seat("North") {
	def getLeft = East
	def getRight = West
	def getCross = South
	def getDirectionRelative(that : Seat)  = {
		that match {
			case South => CrossDirection
			case West => RightDirection
			case East => LeftDirection
		}
	}
}

case object South extends Seat("South"){
	def getLeft  = West
	def getRight = East
	def getCross = North
	def getDirectionRelative(that : Seat) = {
		that match {
			case West => LeftDirection
			case East => RightDirection
			case North => CrossDirection
		}
	}
}

case object West extends Seat("West"){
	def getLeft = North
	def getRight = South
	def getCross = East
	def getDirectionRelative(that : Seat) = {
		that match {
			case North => LeftDirection
			case South => RightDirection
			case East => CrossDirection
		}
	}
}

case object East extends Seat("East"){
	def getLeft = South
	def getRight = North
	def getCross = West
	def getDirectionRelative(that : Seat ) = {
		that match {
			case South => LeftDirection
			case North => RightDirection
			case West  => CrossDirection
		}
	}

}

object Seats {
	val values = List(North,South,East,West)
}


