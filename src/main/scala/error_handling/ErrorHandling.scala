package error_handling

import java.time.YearMonth
import cats.data.ValidatedNec

import scala.util.Try

object ErrorHandling {

  case class PaymentCard(name: String, number: Int, expirationDate: YearMonth, securityCode: Int /* Add parameters as needed */)

  sealed trait ValidationError

  object ValidationError {

    final case object DigitsInName extends ValidationError

    final case object EmptyName extends ValidationError

    final case object InvalidSurname extends ValidationError

    final case object InvalidNumber extends ValidationError

    final case object Expired extends ValidationError

    final case object InvalidSecurityCodeLength extends ValidationError

    final case object NotNumericSecurityCode extends ValidationError

    final case object InvalidDate extends ValidationError

  }

  object PaymentCardValidator {

    import ValidationError._
    import cats.syntax.all._


    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    private def validateName(name: String): AllErrorsOr[String] = {
      def validateNameLetters(name: String): AllErrorsOr[String] = {
        if (name.forall(_.isLetter)) name.validNec
        else DigitsInName.invalidNec
      }

      def validateNameSurname: AllErrorsOr[String] = {
        if (name.split(" ").toList.length > 1) name.validNec
        else InvalidSurname.invalidNec
      }

      def isEmpty: AllErrorsOr[String] = {
        if (name.nonEmpty) name.validNec
        else EmptyName.invalidNec
      }

      isEmpty.andThen(validateNameLetters).productR(validateNameSurname)
    }

    private def validateNumber(number: String): AllErrorsOr[Int] = {
      if (number.matches("^\\d{4} \\d{4} \\d{4} \\d{4}$")) number.toInt.validNec
      else InvalidNumber.invalidNec
    }


    private def validateExpirationDate(expirationDate: String): AllErrorsOr[YearMonth] = {
      val date = raw"(\\d{2})/(\\d{4})".r
      expirationDate match {
        case date(month, year) =>
          Try(YearMonth.of(year.toInt, month.toInt)).toOption
            .map(d => {
              if (d.isAfter(YearMonth.now())) d.validNec else Expired.invalidNec
            }).getOrElse(InvalidDate.invalidNec)
        case _ => InvalidDate.invalidNec
      }
    }

    private def validateSecurityCode(securityCode: String): AllErrorsOr[Int] = {
      def isNumeric: AllErrorsOr[Int] = {
        if (securityCode.forall(_.isDigit)) securityCode.toInt.validNec
        else NotNumericSecurityCode.invalidNec
      }

      def validateSecurityCodeLength: AllErrorsOr[Int] = {
        if (securityCode.length == 3) securityCode.toInt.validNec
        else InvalidSecurityCodeLength.invalidNec
      }

      isNumeric.productR(validateSecurityCodeLength)
    }


    def validate(
                  name: String,
                  number: String,
                  expirationDate: String,
                  securityCode: String,
                ): AllErrorsOr[PaymentCard] =
      (validateName(name), validateNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode)).mapN(PaymentCard)
  }

}
