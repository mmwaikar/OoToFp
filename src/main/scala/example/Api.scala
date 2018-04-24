package example

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel, Writer}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global

object Api {

  type ValidatedNelString[A] = Validated[NonEmptyList[String], A]
  type LoggingValidatedNelString[A] = Writer[Vector[String], ValidatedNelString[A]]

  def ooAddWithLogging(x: Int, y: Int) : Int = {
    println(s"Logging done from within the method: Adding $x and $y")
    x + y
  }

  def fpAdd(x: Int, y: Int) : Writer[Vector[String], Int] = {
    Writer(Vector(s"Adding $x and $y"), x + y)
  }

  def ooDivideWithException(x: Int, y: Int): Int = {
    if (y == 0) throw new Exception("Can't divide by zero.")
    x / y
  }

  def fpDivide(x: Int, y: Int): Try[Int] = {
    Try(x / y)
  }

  def fpValidation(x: Int, y: Int): Validated[NonEmptyList[String], Int] = {
    if (y == 0) Invalid(NonEmptyList.one("Cannot divide by zero"))
    else Valid(x / y)
  }

  def fpLoggingValidation(x: Int, y: Int): Future[LoggingValidatedNelString[Int]] = {
    val logs = Vector(s"Trying to divide $x by $y")

    val toRet: LoggingValidatedNelString[Int] = if (y == 0) {
      val validations = Invalid(NonEmptyList.one("Why divide by zero?"))
      Writer(logs, validations)
    } else {
      val result = x / y
      Writer(logs, Valid(result))
    }

    Future { toRet }
  }
}
