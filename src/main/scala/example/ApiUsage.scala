package example

import cats.data.Validated.{Invalid, Valid}
import cats.data.Writer

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

object ApiUsage extends App {

  loggingUsage()
  exceptionUsage()
  validationUsage()

  futureLoggingValidationUsage(2, 0)
  futureLoggingValidationUsage(14, 2)

  private def loggingUsage() = {
    println()
    println("--- OO vs. FP logging")

    // logging is done inside the method as a side effect
    val x = Api.ooAddWithLogging(2, 3)
    println(x)

    // logging has to be done outside of the method
    val x1: Writer[Vector[String], Int] = Api.fpAdd(2, 3)
    println(s"Logging done outside the method: ${x1.written.mkString(", ")}")
    println(x1.value)
  }

  private def exceptionUsage() = {
    println()
    println("--- OO vs. FP exception handling")

    try {
      val x = Api.ooDivideWithException(2, 0)
      println(x)
    } catch {
      case ex: Exception => println(s"Exception occured: ${ex.getMessage}")
    }

    val x1 = Api.fpDivide(2, 0)
    x1 match {
      case Success(v) => println(v)
      case Failure(ex) => println(s"Exception occured: ${ex.getMessage}")
    }
  }

  private def validationUsage() = {
    println()
    println("--- OO vs. FP validation")

    val x1 = Api.fpValidation(2, 0)
    x1 match {
      case Invalid(x) => println(s"Validation failed: ${x.toList.mkString(", ")}")
      case Valid(x) => println(s"Valid: $x")
    }
  }

  private def futureLoggingValidationUsage(x: Int, y: Int) = {
    println()
    println("--- FP future, logging and validation usage")

    val x1 = Api.fpLoggingValidation(x, y)
    x1 onComplete {
      case Success(lvnsi) =>
        val (logs, validationResult) = lvnsi.run
        println(s"${logs.mkString(System.lineSeparator())}")

        validationResult match {
          case Invalid(x) => println(s"Validation failed: ${x.toList.mkString(", ")}")
          case Valid(x) => println(s"The return value: $x")
        }

      case Failure(ex) => println(s"Some error occurred: ${ex.getMessage}")
    }
  }
}
