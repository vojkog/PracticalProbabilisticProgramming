package chap03

import java.nio.file.{Files,Paths,Path}
import java.io._
import scala.io.Source
import com.cra.figaro.language.{Universe, Constant}
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.algorithm.learning._

object LearningComponent {

  def readEmails(directoryName: String): Map[String, Email] = {
    val directory = Paths.get(directoryName)
    val directoryIterator = Files.newDirectoryStream(directory).iterator()

    var result: Map[String, Email] = Map()
    while (directoryIterator.hasNext()) {
      val nextFile = directoryIterator.next().toFile()
      val fileName = nextFile.getName
      println("Reading " + fileName)
      result += fileName ->  new Email(nextFile)
    }
    result
  }

  def readLabels(labelFileName: String): Map[String, Boolean] = {
    val source = Source.fromFile(labelFileName)
    var result: Map[String, Boolean] = Map()
    for {
      line <- source.getLines()
    } {
      val parts = line.split(' ')
      val isSpam = parts(0) == "1"
      val emailFileName = parts(1)
      result += emailFileName -> isSpam
    }
    result
  }

  def learnMAP(params: PriorParameters): LearnedParameters = {
println("Beginning training")
println("Number of elements: " + Universe.universe.activeElements.length)
    val algorithm = EMWithBP(params.fullParameterList:_*)
val time0 = System.currentTimeMillis()
    algorithm.start()
val time1 = System.currentTimeMillis()
println("Training time: " + ((time1 - time0) / 1000.0))
    val spamProbability = params.spamProbability.MAPValue
    val hasUnusualWordsGivenSpamProbability = params.hasManyUnusualWordsGivenSpamProbability.MAPValue
    val hasUnusualWordsGivenNormalProbability = params.hasManyUnusualWordsGivenNormalProbability.MAPValue
    val unusualWordGivenHasUnusualProbability = params.unusualWordGivenManyProbability.MAPValue
    val unusualWordGivenNotHasUnusualProbability = params.unusualWordGivenFewProbability.MAPValue
    val wordGivenSpamProbabilities =
      for { (word, param) <- params.wordGivenSpamProbabilities }
      yield (word, param.MAPValue)
    val wordGivenNormalProbabilities =
      for { (word, param) <- params.wordGivenNormalProbabilities }
      yield (word, param.MAPValue)
    algorithm.kill()
    new LearnedParameters(
      spamProbability,
      hasUnusualWordsGivenSpamProbability,
      hasUnusualWordsGivenNormalProbability,
      unusualWordGivenHasUnusualProbability,
      unusualWordGivenNotHasUnusualProbability,
      wordGivenSpamProbabilities.toMap,
      wordGivenNormalProbabilities.toMap
    )
  }

  def saveResults(
      fileName: String,
      dictionary: Dictionary,
      learningResults: LearnedParameters
   ) = {
    val file = new File(fileName)
    val output = new PrintWriter(new BufferedWriter(new FileWriter(file)))

    output.println(dictionary.numEmails)
    output.println(learningResults.spamProbability)
    output.println(learningResults.hasManyUnusualWordsGivenSpamProbability)
    output.println(learningResults.hasManyUnusualWordsGivenNormalProbability)
    output.println(learningResults.unusualWordGivenManyProbability)
    output.println(learningResults.unusualWordGivenFewProbability)
    output.println(dictionary.words.length)
    output.println(dictionary.featureWords.length)

    for {
      word <- dictionary.words
    } {
      output.println(word)
      output.println(dictionary.getCount(word))
    }

    for {
      word <- dictionary.featureWords
    } {
      output.println(word)
      output.println(learningResults.wordGivenSpamProbabilities(word))
      output.println(learningResults.wordGivenNormalProbabilities(word))
    }

    output.close()
  }

  def main(args: Array[String]) {
    val trainingDirectoryName = args(0)
    val labelFileName = args(1)
    val learningFileName = args(2)

    val emails = readEmails(trainingDirectoryName)
    val labels = readLabels(labelFileName)
    val dictionary = Dictionary.fromEmails(emails.values)

    val params = new PriorParameters(dictionary)
    val models =
      for { (fileName, email) <- emails }
      yield {
        val model = new LearningModel(dictionary, params)
        email.observeEvidence(model, labels.get(fileName), true)
        model
      }

    val results = learnMAP(params)
    saveResults(learningFileName, dictionary, results)
    println("Done!")
  }
}
