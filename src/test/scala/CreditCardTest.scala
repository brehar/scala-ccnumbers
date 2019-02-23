import org.scalatest.{FunSuite, Matchers}

trait TestStyle extends FunSuite with Matchers

class CreditCardTest extends TestStyle {
  test("Creating a card without passing any number should generate a valid credit card") {
    CreditCard() shouldBe 'valid
  }

  test("Creating a card without passing any number should create a card of class CreditCard.Valid") {
    CreditCard() shouldBe a[CreditCard.Valid]
  }

  test("Creating a card manually by passing a valid number should produce a valid credit card") {
    val validNumber = CreditCard().number

    CreditCard(validNumber) shouldBe 'valid
    noException should be thrownBy CreditCard(validNumber).asInstanceOf[CreditCard.Valid]
  }

  test("Credit card's toString method should mention validity") {
    CreditCard("").toString.toLowerCase should include("invalid")
    CreditCard().toString.toLowerCase should not include "invalid"
  }

  test("All these numbers should be valid") {
    val fakeCards = Set(
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
      "6376567786703901"
    ).map(CreditCard)

    all(fakeCards.map(_.isValid)) shouldBe true
  }

  test("10,000 generated numbers should be valid") {
    val fakeCards = 1 to 10000 map (_ => CreditCard())

    all(fakeCards.map(_.isValid)) shouldBe true
  }
}
