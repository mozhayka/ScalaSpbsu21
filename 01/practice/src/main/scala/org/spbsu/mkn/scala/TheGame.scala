package org.spbsu.mkn.scala

import java.lang.invoke.WrongMethodTypeException

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult
  case class Correct(numTries: Int) extends GuessResult
  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException
  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  def generateNumberString(length: Int): String = {
    val str = scala.util.Random.alphanumeric.take(length).mkString
    if (str.toSet.size == str.length) {
      str
    }
    else {
      generateNumberString(length)
    }
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = {
    var bulls = 0
    var cows = 0
    val n = secret.length
    val m = userInput.length
    if (m != n) {
      throw new WrongNumberLengthException(n, m)
    }
    if (userInput.toSet.size != userInput.length) {
      throw new RepeatingDigitsException
    }
    for (i <- 0 until n if secret(i) == userInput(i))
      bulls += 1
    for (i <- secret if userInput.contains(i))
      cows += 1
    cows -= bulls
    if (bulls == n) {
      Correct(numTries)
    }
    else {
      Incorrect(bulls, cows)
    }
  }

  def main(args: Array[String]): Unit = {
    print("Enter length: ")
    val len = readLine().toInt
    val secret = generateNumberString(len)
    var numTries = 1

    while (true) {
      try {
        val userInput = readLine()
        val ans = validate(secret, userInput, numTries)
        ans match {
          case Correct(numTries) =>
            println(s"correct, numTries: $numTries")
            return
          case Incorrect(bulls, cows) =>
            println(s"bulls: $bulls cows: $cows")
            numTries += 1
        }
      } catch {
        case _: WrongNumberLengthException => println("wrong length")
        case _: RepeatingDigitsException => println("repeating digits")
      }
    }
  }
}
