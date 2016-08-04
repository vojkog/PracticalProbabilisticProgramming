package chap03

import java.io.File
import scala.io.Source

class Email(file: File) {
  def getAllWords() = {
    def getWords(line: String): List[String] = {
      for {
        rawWord <- line.split(Array(' ', '\t', '\n')).toList
        word = rawWord.filter((c: Char) => c.isLetterOrDigit)
        if !word.isEmpty
      } yield word.toLowerCase()
    }

    val source = Source.fromFile(file)("ISO-8859-1")
    val allLines = source.getLines().toList

    val allWordsWithRepeats =
      for {
        line <- allLines
        word <- getWords(line)
      } yield word

    allWordsWithRepeats.toSet
  }

  val allWords: Set[String] = getAllWords()

  def observeEvidence(model: Model, label: Option[Boolean], learning: Boolean) {
    label match {
      case Some(b) => model.isSpam.observe(b)
      case None => ()
    }

    for {
      (word, element) <- model.hasWordElements
    } {
      element.observe(allWords.contains(word))
    }

    val obsNumUnusualWords =
      allWords.filter((word: String) => model.dictionary.isUnusual(word, learning)).size
    val unusualWordFraction = obsNumUnusualWords * Model.binomialNumTrials / allWords.size
    model.numUnusualWords.observe(unusualWordFraction)
  }
}

object Email {
  def main(args: Array[String]) {
    val email = new Email(new File("R:/dev/Book/SpamFilter/TESTING/TEST_00009.eml"))
    println(email.allWords)
  }
}
