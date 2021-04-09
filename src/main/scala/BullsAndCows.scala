import scala.io.StdIn.readLine

object BullsAndCows extends App {

  //https://en.wikipedia.org/wiki/Bulls_and_Cows
  println("Let's start Bulls and Cows game!")



  val playerA = readLine("What is your name, Player A?")
  var playerB = "Player B"

  val isPlayerBComputer = readLine("Do you want to play against computer (Y/N)?").toUpperCase.startsWith("Y")
  if (isPlayerBComputer) playerB = "Computer" else playerB = readLine("What is your name, Player B?")

  val players = Seq(playerA, playerB)
  //TODO add entering player names and using them further
  var playerASecretNumber = readLine("Player A, enter your 4-digit secret number: ")
  var playerBSecretNumber = readLine("Player B, enter your 4-digit secret number: ")
  //TODO do numberValidator for entered secret number. Plus, need to make sure that digits do not repeat according to game rules

  val gameResults = for (p <- players) yield (p, guessingProcess(secretNumberToGuess(p),p))

  if (gameResults.minBy(_._2) == gameResults.maxBy(_._2)) println("No winner - the same number of guesses :) ")
  else println(s"Congratulations, ${gameResults.minBy(_._2)._1}, you won!")


  def guessingProcess(secretNumber: String, player: String): Int = {
    var numberOfGuesses = 0
    var guess = ""
    println(s"It is your turn, $player!")
    while (!guess.equals(secretNumber)) {
      guess = readLine("Make your guess!")
      while (!numberValidator(guess)) guess = readLine(s"Not valid guess. Try once again!")
      var bulls = 0
      var cows = 0
      for (i <- 0 to guess.length-1) {
        if (guess(i).equals(secretNumber(i))) bulls += 1
        else if (secretNumber.contains(guess(i))) cows += 1
      }
      numberOfGuesses += 1
      if (bulls == secretNumber.length) println(s"You got all $bulls bulls with $numberOfGuesses guesses!") else println(s"You got $bulls bulls and $cows cows")
    }
    numberOfGuesses
  }

  def secretNumberToGuess(player: String): String = if (player.equals(players(0))) playerBSecretNumber else playerASecretNumber

  def numberValidator(input: String): Boolean = {
    //checking char by ASCII code value if it is a digit 1 - 9
    val check = for (i <- input if i.toInt > 48 && i.toInt < 58) yield i
    if (check.length == input.length) true else false
  }




}




