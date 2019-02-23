// $COVERAGE-OFF$
object Main {
  def main(args: Array[String]): Unit = {
    code(args)
  }

  private def code(args: Array[String]): Unit = {
    args.headOption.map(CreditCard).map(println).getOrElse(runDemo())
  }

  private def runDemo(): Unit = {
    val validCard: CreditCard.Valid = CreditCard()

    println(validCard)
    println(validCard.number)
    println(validCard.isValid)

    println()

    val invalidCard: CreditCard = CreditCard("1234567812345678")

    println(invalidCard)
    println(invalidCard.number)
    println(invalidCard.isValid)

    println()

    val fakeNumbers = 1 to 10000 map (_ => CreditCard())

    println(fakeNumbers.forall(_.isValid))

    val moreFakeNumbers = Set(
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

    val (_, invalid) = moreFakeNumbers.partition(_.isValid)

    if (invalid.nonEmpty) {
      println()
      invalid foreach println
    }

    println()
    println(
      "You can also pass in the credit card number as a command line argument like this: run [number]")
  }
}
// $COVERAGE-ON$
