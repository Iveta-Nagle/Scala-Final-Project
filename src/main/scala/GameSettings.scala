import java.io.{File, FileInputStream}

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor

import scala.beans.BeanProperty

class GameSettings {
  @BeanProperty var playerA = "Anna"
  @BeanProperty var playerB = "Peteris"
  @BeanProperty var numberLength = 4
  @BeanProperty var databaseLocation = "./src/resources/db/bullsandcows.db"

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
  val dbUrl: String = s"jdbc:sqlite:${settings.databaseLocation}" //for now we only support sqlite

}

class GameState(var playerA: String = GameConstants.playerA,
                var playerB: String = GameConstants.playerB,
                var numberLength: Int = GameConstants.numberLength,
                var isPlayerBComputer: Boolean = false,
                var guessesA: Int = 0,
                var guessesB: Int = 0) {
  println(s"Initialized our GameState object with $numberLength-digit numbers")
}
