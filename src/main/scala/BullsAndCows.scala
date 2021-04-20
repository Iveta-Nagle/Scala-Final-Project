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
  var numberLength: Int = settings.numberLength
}

class GameState(var playerA: String = GameConstants.playerA,
                var playerB: String = GameConstants.playerB,
                var numberLength: Int = GameConstants.numberLength,
                var isPlayerBComputer: Boolean = false) {
  println(s"Instantiated our GameState object with $numberLength-digit numbers")
}

object BullsAndCows extends App {

  //https://en.wikipedia.org/wiki/Bulls_and_Cows
  println("Let's start Bulls and Cows game!")

  val playerA = readLine(s"What is your name, Player A? Press Enter to use default ${GameConstants.playerA}")

  val state = if (playerA.length == 0) new GameState()
              else new GameState(playerA)
//
//if (readLine("Do you want to play against computer (Y/N)?").toUpperCase.startsWith("Y")) {
//  state.playerB = "Computer"
//  state.isPlayerBComputer = true
//} else state.playerB = readLine(s"What is your name, Player B? Press Enter to use default ${GameConstants.playerB}")

if (!readLine("Select game mode:\n 1 - single player (guess a computer generated number)\n 2 - 2 players (guess each other's numbers)").contains("2")) { //default is single player
  state.playerB = "Computer"
  state.isPlayerBComputer = true
} else state.playerB = readLine(s"What is your name, Player B? Press Enter to use default ${GameConstants.playerB}")

  if (state.playerB.length == 0) state.playerB = GameConstants.playerB

  val players = Seq(state.playerA, state.playerB)

  state.numberLength = if (readLine(s"Do you want to change length of secret number? (Y/N)")
                          .toUpperCase
                          .startsWith("Y"))
                        readLine("Enter length of secret number: ").toInt
  else GameConstants.numberLength

  val r = new Random()

  val invalidNumberMessage = s"Not a valid number. Enter a number with ${state.numberLength} distinct digits."

  var playerBSecretNumber = ""
  var playerASecretNumber = ""
  if (state.isPlayerBComputer) playerBSecretNumber = computerSecretNumber() else {
    playerASecretNumber = readLine(s"${state.playerA}, enter your ${state.numberLength}-digit secret number: ")
    while (!numberValidator(playerASecretNumber)) playerASecretNumber = readLine(invalidNumberMessage)
    for (_ <- Range(0,30)) println("\n") // So that other player does not see the number (could not get readPassword to work)
    playerBSecretNumber = readLine(s"${state.playerB}, enter your ${state.numberLength}-digit secret number: ")
    while (!numberValidator(playerBSecretNumber)) playerBSecretNumber = readLine(invalidNumberMessage)
    for (_ <- Range(0,30)) println("\n") // So that other player does not see the number (could not get readPassword to work)
  }

  if (!state.isPlayerBComputer) {
    val gameResults = for (p <- players) yield (p, guessingProcess(secretNumberToGuess(p, state.isPlayerBComputer), p))
    if (gameResults.minBy(_._2) == gameResults.maxBy(_._2)) println("No winner - the same number of guesses :) ")
      else println(s"Congratulations, ${gameResults.minBy(_._2)._1}, you won!")
  } else guessingProcess(secretNumberToGuess(playerBSecretNumber,state.isPlayerBComputer),state.playerA)

  def guessingProcess(secretNumber: String, player: String): Int = {
    var numberOfGuesses = 0
    var guess = ""
    println(s"It is your turn, $player!")
    while (!guess.equals(secretNumber)) {
      guess = readLine("Make your guess!")
      while (!numberValidator(guess)) guess = readLine(invalidNumberMessage)
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
    else if (player.equals(state.playerA)) playerBSecretNumber else playerASecretNumber
  }

  def numberValidator(input: String, numberLength: Int = state.numberLength): Boolean = {
    //checking char by ASCII code value if it is a digit 1 - 9
    val check = for (i <- input if i.toInt > 48 && i.toInt < 58 ) yield i
    if (check.length == input.length && check.length == numberLength && check.distinct.length == check.length) true else false
    //distinct size checks number of unique digits  - according to rules all must be different
  }




}




