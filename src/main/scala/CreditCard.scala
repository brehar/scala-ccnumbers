sealed trait CreditCard extends Product with Serializable {
  import CreditCard.{ Valid, split }

  def number: String
  final def isValid: Boolean = isInstanceOf[Valid]
  final def isNotValid: Boolean = !isValid
  final override def toString: String =
    if (isNotValid) {
      val invalid = Console.RED + "Invalid" + Console.RESET

      s"$invalid credit card number $number"
    } else {
      val valid = Console.GREEN + "Valid" + Console.RESET
      val (payload, checkDigit) = split(number)

      s"$valid credit card number $number with payload $payload and check digit $checkDigit"
    }
}

object CreditCard extends (String => CreditCard) {
  object Invalid {
    private[CreditCard] def apply(number: String): Invalid = new Invalid(number)
  }

  final case class Invalid private (number: String) extends CreditCard

  object Valid {
    private[CreditCard] def apply(number: String): Valid = new Valid(number)
  }

  final case class Valid private (number: String) extends CreditCard

  def apply(number: String): CreditCard =
    if (isValid(number)) Valid(number)
    else Invalid(number)

  private val CHECK_DIGIT_LENGTH = 1
  private val MINIMUM_LENGTH = 13
  private val MAXIMUM_LENGTH = 19

  private def isValid(number: String): Boolean =
    number != null && number.nonEmpty && number.forall(Character.isDigit) && (MINIMUM_LENGTH to MAXIMUM_LENGTH)
      .contains(number.length) && doesMathCheckOut(number)

  private def doesMathCheckOut(number: String): Boolean = {
    val (payload, checkDigit) = split(number)
    val sum = luhn(payload) + checkDigit

    sum % 10 == 0
  }

  private def luhn(payload: String): Int =
    payload.reverse
      .map(_.toString.toInt)
      .zipWithIndex
      .map {
        case (digit, index) =>
          if (index % 2 == 0) digit * 2
          else digit
      }
      .map { number =>
        if (number > 9) number - 9
        else number
      }
      .sum

  private def split(number: String): (String, Int) = {
    val payload = number.dropRight(CHECK_DIGIT_LENGTH)
    val checkDigit = number.takeRight(CHECK_DIGIT_LENGTH).toInt

    payload -> checkDigit
  }

  def apply(): Valid = Valid(generatedNumber)

  private def generatedNumber: String = {
    val payload = {
      import scala.util.Random

      val min: Int = MINIMUM_LENGTH - CHECK_DIGIT_LENGTH
      val max: Int = MAXIMUM_LENGTH - CHECK_DIGIT_LENGTH
      val length: Int = min + Random.nextInt((max - min) + 1)

      def randomDigit: Int = Random.nextInt(10)

      (1 to length).map(_ => randomDigit).mkString
    }
    val checkDigit: Int = (10 - (luhn(payload) % 10)) % 10
    val number = payload + checkDigit

    if (isValid(number)) number
    else
      // $COVERAGE-OFF$
      sys.error(s"Bug: generated invalid number $number")
    // $COVERAGE-ON$
  }
}
