import scala.io.StdIn.readLine

object BullsAndCows extends App {

  //https://en.wikipedia.org/wiki/Bulls_and_Cows
  println("Let's start Bulls and Cows game!")
  val players = Seq("Player A", "Player B")
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
      var bullCount = 0
      var cowCount = 0
      for (i <- 0 to guess.length-1) {
        if (guess(i).equals(secretNumber(i))) bullCount += 1
        else if (secretNumber.contains(guess(i))) cowCount += 1
      }
      println(s"You got $bullCount bulls and $cowCount cows")
      numberOfGuesses += 1
    }
    numberOfGuesses
  }

  def secretNumberToGuess(player: String): String = if (player.equals(players(0))) playerBSecretNumber else playerASecretNumber

  def numberValidator(input: String): Boolean = {
    //checking char by ASCII code value if it is a digit 1 - 9
    val check = for (i <- input if i.toInt > 48 && i.toInt < 58) yield i
    if (check.length == 4) true else false
  }




}



