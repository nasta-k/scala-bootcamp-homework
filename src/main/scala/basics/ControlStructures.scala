package basics


import scala.io.Source
import scala.util.{Failure, Success, Try}

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
    import Command._
    val commandList: List[String] = x.split(" ").toList.filter(_ != "")
    commandList match {
      case _ :: Nil => Left(ErrorMessage(s"$errorPrefix empty arguments"))
      case x :: xs => Try(xs.map(_.toDouble)) match {
        case Success(args) => (x, args) match {
          case ("divide", arg) => arg match {
            case List(dividend, divisor) => Right(Divide(dividend, divisor))
            case _ => Left(ErrorMessage(s"$errorPrefix invalid divide arguments"))
          }
          case ("sum", args) => Right(Sum(args))
          case ("average", args) => Right(Average(args))
          case ("min", args) => Right(Min(args))
          case ("max", args) => Right(Max(args))
        }
        case Failure(_) => Left(ErrorMessage(s"$errorPrefix invalid arguments"))
      }
      case Nil => Left(ErrorMessage(s"$errorPrefix empty data"))
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

  val decimalFormat = new DecimalFormat("#.###")

  private def formatOutput(number: Double) = decimalFormat.format(number)

  def renderResult(x: Result): String = {
    val numbers = x.numbers.map(formatOutput)
    x.command match {
      case "divide" => s"${numbers.head} divided by ${numbers.tail.head} is ${formatOutput(x.result)}"
      case _ => s"the ${x.command} of${numbers.foldLeft("")((a, b) => a + " " + b)} is ${formatOutput(x.result)}"
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
