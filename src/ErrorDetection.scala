import java.io.File
import scala.io.Source
import scala.collection.mutable.HashMap

object ErrorDetection extends App {

  val matrix = constructMatrix()

  var done = false
  while (!done) {
    print("Please enter a word or 'q' to exit: ")
    val line = Console.readLine
    if (!line.equals("q")) {
      if (isMisspelled(line))
        println("The word is Not Valid")
      else
        println("The word is Valid")
    } else {
      println("exiting...")
      done = true
    }
  }

  /*********************************************/

  // Create the Markov Matrix
  def constructMatrix(): HashMap[Char, Int] = {
    val training_set = Source.fromFile(new File("input/training.txt"))
    var matrix = new HashMap[Char, Int]()

    training_set.getLines.foreach(line => line.foreach(char =>
      if (matrix.contains(char)) matrix(char) += 1 else matrix(char) = 0))
    return matrix
  }

  // Predicts whether the word was misspelled
  def isMisspelled(word: String): Boolean = {
    word.foreach(char => if (!matrix.contains(char)) return true)
    return false
  }

}