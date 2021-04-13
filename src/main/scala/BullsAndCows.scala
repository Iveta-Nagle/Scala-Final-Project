import java.io.{File, FileInputStream}

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.beans.BeanProperty
import scala.io.StdIn.readLine
import scala.util.Random

class GameSettings {
  @BeanProperty var playerA = "Anna"
  @BeanProperty var playerB = "Peteris"
  @BeanProperty var numberLength = 4

  override def toString: String = s"Player A: $playerA, player B: $playerB, length of secret number: $numberLength"
}

object GameConstants {
  val relativePath = "config.yaml"
  val input = new FileInputStream(new File(relativePath))
  val yaml = new Yaml(new Constructor(classOf[GameSettings]))
  val settings: GameSettings = yaml.load(input).asInstanceOf[GameSettings]
  println(settings)
  var playerA: String = settings.playerA
  var playerB: String = settings.playerB
  var numberLength: Int = 4
}

class GameState(var playerA: String = GameConstants.playerA,
                var playerB: String = GameConstants.playerB,
                var numberLength: Int = GameConstants.numberLength) {
  println(s"Instantiated our GameState object with $numberLength-digit numbers")
}

object BullsAndCows extends App {

  //https://en.wikipedia.org/wiki/Bulls_and_Cows
  println("Let's start Bulls and Cows game!")


  //TODO make GameSettings class
  //TODO make GameConstants object
  val playerA = readLine(s"What is your name, Player A? Press Enter to use default ${GameConstants.playerA}")
  var playerB = "Player B"

  val isPlayerBComputer = readLine("Do you want to play against computer (Y/N)?").toUpperCase.startsWith("Y")
  if (isPlayerBComputer) playerB = "Computer" else playerB = readLine("What is your name, Player B?")

  val players = Seq(playerA, playerB)
  val numberLength = 4

  val r = new Random()

  var playerBSecretNumber = ""
  var playerASecretNumber = ""
  if (isPlayerBComputer) playerBSecretNumber = computerSecretNumber() else {
    playerASecretNumber = readLine(s"$playerA, enter your $numberLength-digit secret number: ")
    while (!numberValidator(playerASecretNumber)) playerASecretNumber = readLine(s"Not valid number. Enter it once again!")
    playerBSecretNumber = readLine(s"$playerB, enter your $numberLength-digit secret number: ")
    while (!numberValidator(playerBSecretNumber)) playerBSecretNumber = readLine(s"Not valid number. Enter it once again!")
  }

  //TODO allow to choose number of digits (right now the game is made with 4 digit number)

  if (!isPlayerBComputer) {
    val gameResults = for (p <- players) yield (p, guessingProcess(secretNumberToGuess(p, isPlayerBComputer), p))
    if (gameResults.minBy(_._2) == gameResults.maxBy(_._2)) println("No winner - the same number of guesses :) ")
      else println(s"Congratulations, ${gameResults.minBy(_._2)._1}, you won!")
  } else guessingProcess(secretNumberToGuess(playerBSecretNumber,isPlayerBComputer),playerA)

  def guessingProcess(secretNumber: String, player: String): Int = {
    var numberOfGuesses = 0
    var guess = ""
    println(s"It is your turn, $player!")
    while (!guess.equals(secretNumber)) {
      guess = readLine("Make your guess!")
      while (!numberValidator(guess)) guess = readLine(s"Not valid guess. Enter it once again!")
      var bulls = 0
      var cows = 0
      for (i <- 0 until guess.length) {
        if (guess(i).equals(secretNumber(i))) bulls += 1
        else if (secretNumber.contains(guess(i))) cows += 1
      }
      numberOfGuesses += 1
      if (bulls == secretNumber.length) println(s"You got all $bulls bulls with $numberOfGuesses guesses!") else println(s"You got $bulls bulls and $cows cows")
    }
    numberOfGuesses
  }

  def computerSecretNumber (): String = {
    var secretNumber = ""
    val list = (1 to 9).toList
    val randomDigits = r.shuffle(list).take(4)
    for (c <- randomDigits) secretNumber+= c.toString
    //println(s"Computer secretNumber: $secretNumber")
    secretNumber
  }

  def secretNumberToGuess(player: String, isPlayerBComputer: Boolean): String = {
    if (isPlayerBComputer) playerBSecretNumber
    else if (player.equals(playerA)) playerBSecretNumber else playerASecretNumber
  }

  def numberValidator(input: String): Boolean = {
    //checking char by ASCII code value if it is a digit 1 - 9
    val check = for (i <- input if i.toInt > 48 && i.toInt < 58 ) yield i
    if (check.length == input.length && check.distinct.length == check.length) true else false
    //distinct size checks number of unique digits  - according to rules all must be different
  }




}




