import java.sql.{Connection, DriverManager, PreparedStatement}

import scala.io.StdIn.readLine
import scala.util.Random

object BullsAndCows extends App {

  //https://en.wikipedia.org/wiki/Bulls_and_Cows
  println("Let's start Bulls and Cows game! \n")

  val conn = DriverManager.getConnection(GameConstants.dbUrl)

  /** Creating database table if doesn't exist.
   * Will be used for saving game results.
   */
  migrateTable(conn)

  val playerA = readLine(s"What is your name, Player A? Press Enter to use default ${GameConstants.playerA}")

  val state = if (playerA.length == 0) new GameState()
              else new GameState(playerA)

 /** Selecting game mode and getting player names.
 * Default is 1 player mode against computer.
  */
if (!readLine("Select game mode:\n 1 - single player (guess a computer generated number)\n 2 - 2 players (guess each other's numbers)").contains("2")) {
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

  /**Error message for invalid entered number.
   * Will be used for entering secret numbers and guesses.
   */
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

  /**Playing the game.
   * Uses guessing process method.
   * Saves results and checks winner if 2-players mode.
   * Prints out winner if 2-players mode.
   */
  if (!state.isPlayerBComputer) {
    val gameResults = for (p <- players) yield (p, guessingProcess(secretNumberToGuess(p, state.isPlayerBComputer), p))
    state.guessesA = gameResults.head._2
    state.guessesB = gameResults(1)._2
    if (gameResults.minBy(_._2) == gameResults.maxBy(_._2)) println("No winner - the same number of guesses :) ")
      else println(s"Congratulations, ${gameResults.minBy(_._2)._1}, you won!")
    }
      else state.guessesA = guessingProcess(secretNumberToGuess(playerBSecretNumber,state.isPlayerBComputer),state.playerA)


  /**Inserting game stats into database.
   * Takes number of guesses when guessing process has been finished.
   */
  insertGameStats(conn)

  /** Guessing the secret number.
   * Player enters guess that is checked by validator.
   * Prints number of cows and bulls in each guess.
   * Runs until player has gotten all bulls.
   * @param player which is doing the guessing process.
   * @param secretNumber which number player has to guess.
   * @return number of guesses with which player guessed the secret number.
   */
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
      if (bulls == secretNumber.length) println(s"You got all $bulls bulls with $numberOfGuesses guesses! \n") else println(s"You got $bulls bulls and $cows cows \n")
    }
    numberOfGuesses
  }

  /** Generates computer's secret number.
   * Takes only distinct digits.
   * @param numberLength how long secret number method needs to generate. By default settings 4.
   * @return secret number that player will need to guess.
   * Prints out the number only for testing purposes.
   */
  def computerSecretNumber (numberLength: Int = state.numberLength): String = {
    var secretNumber = ""
    val list = (1 to 9).toList
    val randomDigits = r.shuffle(list).take(numberLength)
    for (c <- randomDigits) secretNumber+= c.toString
    println(secretNumber)
    secretNumber
  }

  /** Checks secret number that has to be guessed in the game.
   * Takes only distinct digits.
   * @param player which player's turn it is to guess. Takes the opposite player's secret number for guessing.
   * @param isPlayerBComputer returns computer secret number for guessing.
   * @return secret number that player will need to guess.
   */
  def secretNumberToGuess(player: String, isPlayerBComputer: Boolean): String = {
    if (isPlayerBComputer) playerBSecretNumber
    else if (player.equals(state.playerA)) playerBSecretNumber else playerASecretNumber
  }

  /** Checks if player entered correct number.
   * Will be used for process of entering secret numbers and guesses.
   * Checks char by ASCII code value if it is a digit 1 - 9
   * Checks if entered number has only distinct digits.
   * @param input what user entered.
   * @param numberLength how long the number can be. By default settings 4.
   * @return if user entered number of distinct digits and for set up number length.
   */
  def numberValidator(input: String, numberLength: Int = state.numberLength): Boolean = {
    val check = for (i <- input if i.toInt > 48 && i.toInt < 58 ) yield i
    if (check.length == input.length && check.length == numberLength && check.distinct.length == check.length) true else false
  }


  /** Creates table for game results in database.
   * Creates a Statement object for sending SQL statements to the database.
   */
  def migrateTable(conn:Connection): Int = {
    val statement = conn.createStatement()

    val sql =
      """
        |CREATE TABLE IF NOT EXISTS gameStats (
        |	gameID INTEGER PRIMARY KEY,
        | playerNameA TEXT NOT NULL,
        | playerNameB TEXT NOT NULL,
        |	numberLength INTEGER DEFAULT 0,
        | guessesA INTEGER DEFAULT 0,
        | guessesB INTEGER DEFAULT 0
        |);
        |""".stripMargin

    statement.executeUpdate(sql)
  }


  /** Inserts game results in database.
   * Used after guessing process is finished.
   */
  def insertGameStats(connection: Connection):Unit = {
    val insertSql =
      """
        |INSERT INTO gameStats(
        |   playerNameA,
        |   playerNameB,
        |   numberLength,
        |   guessesA,
        |   guessesB)
        |   VALUES(?,?,?,?,?)
      """.stripMargin

      val preparedStmt: PreparedStatement = connection.prepareStatement(insertSql)
      preparedStmt.setString(1, state.playerA)
      preparedStmt.setString(2, state.playerB)
      preparedStmt.setInt(3, state.numberLength)
      preparedStmt.setInt(4, state.guessesA)
      preparedStmt.setInt(5, state.guessesB)
      preparedStmt.execute
      preparedStmt.close()

  }




}




