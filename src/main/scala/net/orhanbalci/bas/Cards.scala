package net.orhanbalci.bas

sealed abstract class CardType(val name: String)
case object Spades   extends CardType("Spades")
case object Hearts   extends CardType("Hearts")
case object Diamonds extends CardType("Diamonds")
case object Clubs    extends CardType("Clubs")

sealed abstract class CardNumber(val name: String, val order: Int) extends Ordered[CardNumber] {
  def compare(that: CardNumber) = this.order - that.order
}
case object Ace   extends CardNumber("Ace", 14)
case object King  extends CardNumber("King", 13)
case object Queen extends CardNumber("Queen", 12)
case object Jack  extends CardNumber("Jack", 11)
case object Ten   extends CardNumber("Ten", 10)
case object Nine  extends CardNumber("Nine", 9)
case object Eight extends CardNumber("Eight", 8)
case object Seven extends CardNumber("Seven", 7)
case object Six   extends CardNumber("Six", 6)
case object Five  extends CardNumber("Five", 5)
case object Four  extends CardNumber("Four", 4)
case object Three extends CardNumber("Three", 3)
case object Two   extends CardNumber("Two", 2)

class Card(val cardType: CardType, val cardNumber: CardNumber)
case object AceOfSpades   extends Card(Spades, Ace)
case object KingOfSpades  extends Card(Spades, King)
case object QueenOfSpades extends Card(Spades, Queen)
case object JackOfSpades  extends Card(Spades, Jack)
case object TenOfSpades   extends Card(Spades, Ten)
case object NineOfSpades  extends Card(Spades, Nine)
case object EightOfSpades extends Card(Spades, Eight)
case object SevenOfSpades extends Card(Spades, Seven)
case object SixOfSpades   extends Card(Spades, Six)
case object FiveOfSpades  extends Card(Spades, Five)
case object FourOfSpades  extends Card(Spades, Four)
case object ThreeOfSpades extends Card(Spades, Three)
case object TwoOfSpades   extends Card(Spades, Two)

case object AceOfHearts   extends Card(Hearts, Ace)
case object KingOfHearts  extends Card(Hearts, King)
case object QueenOfHearts extends Card(Hearts, Queen)
case object JackOfHearts  extends Card(Hearts, Jack)
case object TenOfHearts   extends Card(Hearts, Ten)
case object NineOfHearts  extends Card(Hearts, Nine)
case object EightOfHearts extends Card(Hearts, Eight)
case object SevenOfHearts extends Card(Hearts, Seven)
case object SixOfHearts   extends Card(Hearts, Six)
case object FiveOfHearts  extends Card(Hearts, Five)
case object FourOfHearts  extends Card(Hearts, Four)
case object ThreeOfHearts extends Card(Hearts, Three)
case object TwoOfHearts   extends Card(Hearts, Two)

case object AceOfDiamonds   extends Card(Diamonds, Ace)
case object KingOfDiamonds  extends Card(Diamonds, King)
case object QueenOfDiamonds extends Card(Diamonds, Queen)
case object JackOfDiamonds  extends Card(Diamonds, Jack)
case object TenOfDiamonds   extends Card(Diamonds, Ten)
case object NineOfDiamonds  extends Card(Diamonds, Nine)
case object EightOfDiamonds extends Card(Diamonds, Eight)
case object SevenOfDiamonds extends Card(Diamonds, Seven)
case object SixOfDiamonds   extends Card(Diamonds, Six)
case object FiveOfDiamonds  extends Card(Diamonds, Five)
case object FourOfDiamonds  extends Card(Diamonds, Four)
case object ThreeOfDiamonds extends Card(Diamonds, Three)
case object TwoOfDiamonds   extends Card(Diamonds, Two)

case object AceOfClubs   extends Card(Clubs, Ace)
case object KingOfClubs  extends Card(Clubs, King)
case object QueenOfClubs extends Card(Clubs, Queen)
case object JackOfClubs  extends Card(Clubs, Jack)
case object TenOfClubs   extends Card(Clubs, Ten)
case object NineOfClubs  extends Card(Clubs, Nine)
case object EightOfClubs extends Card(Clubs, Eight)
case object SevenOfClubs extends Card(Clubs, Seven)
case object SixOfClubs   extends Card(Clubs, Six)
case object FiveOfClubs  extends Card(Clubs, Five)
case object FourOfClubs  extends Card(Clubs, Four)
case object ThreeOfClubs extends Card(Clubs, Three)
case object TwoOfClubs   extends Card(Clubs, Two)

object Deck {
  val values = Set(AceOfSpades,
                   KingOfSpades,
                   QueenOfSpades,
                   JackOfSpades,
                   TenOfSpades,
                   NineOfSpades,
                   EightOfSpades,
                   SevenOfSpades,
                   SixOfSpades,
                   FiveOfSpades,
                   FourOfSpades,
                   ThreeOfSpades,
                   TwoOfSpades,
                   AceOfHearts,
                   KingOfHearts,
                   QueenOfHearts,
                   JackOfHearts,
                   TenOfHearts,
                   NineOfHearts,
                   EightOfHearts,
                   SevenOfHearts,
                   SixOfHearts,
                   FiveOfHearts,
                   FourOfHearts,
                   ThreeOfHearts,
                   TwoOfHearts,
                   AceOfDiamonds,
                   KingOfDiamonds,
                   QueenOfDiamonds,
                   JackOfDiamonds,
                   TenOfDiamonds,
                   NineOfDiamonds,
                   EightOfDiamonds,
                   SevenOfDiamonds,
                   SixOfDiamonds,
                   FiveOfDiamonds,
                   FourOfDiamonds,
                   ThreeOfDiamonds,
                   TwoOfDiamonds,
                   AceOfClubs,
                   KingOfClubs,
                   QueenOfClubs,
                   JackOfClubs,
                   TenOfClubs,
                   NineOfClubs,
                   EightOfClubs,
                   SevenOfClubs,
                   SixOfClubs,
                   FiveOfClubs,
                   FourOfClubs,
                   ThreeOfClubs,
                   TwoOfClubs)
}
