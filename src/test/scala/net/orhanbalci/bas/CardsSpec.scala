
import net.orhanbalci.bas._
import org.scalatest.FunSuite

class CardsSpec extends FunSuite {

	test("Card(Spades, Ace) should equal AceOfSpades"){
		assert(new Card(Spades, Ace) equals AceOfSpades)
	}
}
