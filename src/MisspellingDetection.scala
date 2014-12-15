import java.io.File
import scala.io.Source
import scala.collection.mutable.HashMap

object MisspellingDetection extends App {

  val frequencies = new HashMap[Char, Int]
  val matrix = constructMatrix()
  val thresh = .1

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
    println("-------------------\n")
  }

  /*********************************************/

  // Create the Markov Matrix
  def constructMatrix(): HashMap[(Char, Char), Int] = {
    val training_set = Source.fromFile(new File("input/training.txt"))("UTF-8")
    var matrix = new HashMap[(Char, Char), Int]()

    training_set.getLines.foreach { line =>

      var pchar: Character = null
      line.foreach { char =>
        if (pchar != null) {

          // Add to the Matrix
          if (matrix.contains((pchar, char)))
            matrix((pchar, char)) += 1
          else
            matrix((pchar, char)) = 1

          // Count the frequencies
          if (frequencies.contains(pchar))
            frequencies(pchar) += 1
          else
            frequencies(pchar) = 1
        }
        pchar = char
      }
    }
    return matrix
  }

  // Predicts whether the word was misspelled
  def isMisspelled(word: String): Boolean = {
    var pchar: Character = null
    word.foreach { char =>
      if (pchar != null) {
        // transition count / total transitions
        val score = ((matrix(pchar, char).toDouble) / (frequencies(pchar).toDouble))
        if (!matrix.contains(pchar, char) || score < thresh) {
          println("Chance of Misspelling is " + (1 - score))
          return true
        }
      }
      pchar = char
    }
    return false
  }

}