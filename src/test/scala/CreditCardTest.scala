import org.scalacheck.{ Arbitrary, Gen }
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FunSuite, Matchers }

trait TestStyle extends FunSuite with Matchers with PropertyChecks

class CreditCardTest extends TestStyle {
  val genValid: Gen[CreditCard] = Arbitrary.arbitrary[Unit].map(_ => CreditCard())

  implicit val arbitraryValid: Arbitrary[CreditCard.Valid] = Arbitrary(
    genValid.map(_.asInstanceOf[CreditCard.Valid]))

  test("Creating a card without passing any number should generate a valid credit card") {
    forAll { validCard: CreditCard.Valid =>
      validCard shouldBe 'valid
    }
  }

  test("Creating a card manually by passing a valid number should produce a valid credit card") {
    forAll { validCard: CreditCard.Valid =>
      val validNumber = validCard.number

      CreditCard(validNumber) shouldBe 'valid
      noException should be thrownBy CreditCard(validNumber).asInstanceOf[CreditCard.Valid]
    }
  }

  test("Credit card's toString method should mention validity") {
    CreditCard("").toString.toLowerCase should include("invalid")

    forAll { validCard: CreditCard.Valid =>
      validCard.toString.toLowerCase should not include "invalid"
    }
  }

  test("All these numbers should be valid") {
    val fakeValidCards = Seq(
      "4716705036623214",
      "4539463559309291",
      "4489308492298937136",
      "2221009933502510",
      "5452404915102459",
      "2720999849822810",
      "372240265000630",
      "345361978930139",
      "343693440756607",
      "6011146180917572",
      "6011644224343896",
      "6011388721616743716",
      "3539871018824357",
      "3539016143128726",
      "3529209014562680376",
      "5454402640185912",
      "5553188840989092",
      "5598099595069740",
      "30529957975883",
      "30575990605052",
      "30550480632163",
      "36863025127647",
      "36760132774826",
      "36162639079058",
      "0604060145237479",
      "0604169139075462",
      "0604721322977290",
      "4508346411062538",
      "4508672880691741",
      "4026540496718609",
      "6383589253369421",
      "6396181399415584",
      "6376567786703901").map(CreditCard)

    val gen: Gen[CreditCard] = Gen.oneOf(fakeValidCards)

    forAll(gen) { validCard: CreditCard =>
      validCard shouldBe 'valid
    }
  }
}
