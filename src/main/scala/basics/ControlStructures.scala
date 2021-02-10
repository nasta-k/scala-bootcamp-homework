package basics


import scala.io.Source

object ControlStructures {

  sealed trait Command

  object Command {

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

  }

  final case class ErrorMessage(value: String)

  val errorPrefix: String = "Error:"

  final case class Result(command: String, numbers: List[Double], result: Double)

  def parseCommand(x: String): Either[ErrorMessage, Command] = {
    val commandList: List[String] = x.split(" ").toList.filter(_ != "")
    try {
      val doubleTail: List[Double] = commandList.tail.map(_.toDouble)
      commandList match {
        case _ :: Nil => Left(ErrorMessage(s"$errorPrefix empty data"))
        case "divide" :: _ => Right(Command.Divide(doubleTail.head, doubleTail(1)))
        case "sum" :: _ => Right(Command.Sum(doubleTail))
        case "average" :: _ => Right(Command.Average(doubleTail))
        case "min" :: _ => Right(Command.Min(doubleTail))
        case "max" :: _ => Right(Command.Max(doubleTail))
        case _ => Left(ErrorMessage(s"$errorPrefix invalid command"))
      }
    }
    catch {
      case _: NumberFormatException => Left(ErrorMessage(s"$errorPrefix invalid data"))
    }
  }

  def calculate(x: Command): Either[ErrorMessage, Result] = x match {
    case Command.Divide(dividend, divisor) => divisor match {
      case 0 => Left(ErrorMessage(s"$errorPrefix zero division"))
      case _ => Right(Result("divide", List(dividend, divisor), dividend / divisor))
    }
    case Command.Sum(numbers) => Right(Result("sum", numbers, numbers.sum))
    case Command.Average(numbers) => Right(Result("average", numbers, numbers.sum / numbers.length))
    case Command.Min(numbers) => Right(Result("minimum", numbers, numbers.min))
    case Command.Max(numbers) => Right(Result("maximum", numbers, numbers.max))
  }

  import java.text.DecimalFormat

  val decimalFormat = new DecimalFormat("#.#")

  def renderResult(x: Result): String = {
    val numbers = x.numbers.map(decimalFormat.format)
    x.command match {
      case "divide" => s"${numbers.head} divided by ${numbers.tail.head} is ${x.result.toInt}"
      case _ => s"the ${x.command} of${numbers.foldLeft("")((a, b) => a + " " + b)} is ${x.result.toInt}"
    }
  }

  def process(x: String): String = {
    (for {
      command <- parseCommand(x)
      result <- calculate(command)
    } yield renderResult(result)) match {
      case Right(value) => value
      case Left(error) => error.value
    }
  }


  def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}