object CardType{
	sealed trait EnumVal
	case object Spades extends EnumVal
	case object Hearts extends EnumVal
	case object Diamonds extends EnumVal
	case object Clubs extends EnumVal
}


object Suit {
	//sealed abstract class Card(CardType)
}