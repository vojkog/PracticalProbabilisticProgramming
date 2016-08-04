package chap13

import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.library.compound.If
import com.cra.figaro.language.Element
import com.cra.figaro.algorithm.sampling.MetropolisHastings
import com.cra.figaro.algorithm.sampling.ProposalScheme

object BayesianLearning {
  class Email(val text: List[String], val label: String)

  // In Chapter 3, the emails were drawn automatically from the training and test sets.
  // They are hardcoded here to keep the example simple.
  val trainingEmail1 = new Email(List("Hello, would you like to receive a free book?"), "spam")
  val trainingEmail2 = new Email(List("Please reply to my question by Tuesday!"), "normal")
  val trainingEmail3 = new Email(List("Hello, I have a question about your book."), "normal")
  val trainingEmails = List(trainingEmail1, trainingEmail2, trainingEmail3)

  // Illustrative list of feature words. In Chapter 3, the feature words were derived from the training emails.
  val featureWords = List("hello", "reply", "question", "free", "book")
  val spamProbability = Beta(2,3)

  val wordGivenSpamProbabilities = featureWords.map(word => (word, Beta(2,2))).toMap
  val wordGivenNormalProbabilities = featureWords.map(word => (word, Beta(2,2))).toMap

  class EmailModel {
    val isSpam = Flip(spamProbability)

    val hasWordElements = {
      for { word <- featureWords } yield {
        val givenSpamProbability = wordGivenSpamProbabilities(word)
        val givenNormalProbability = wordGivenNormalProbabilities(word)
        val hasWord =
          If(isSpam, Flip(givenSpamProbability), Flip(givenNormalProbability))
        (word, hasWord)
      }
    }

    val hasWord = hasWordElements.toMap
  }

  for { email <- trainingEmails } {
    val model = new EmailModel
    for { word <- featureWords } {
      model.hasWord(word).observe(email.text.contains(word))
    }
    model.isSpam.observe(email.label == "spam")
  }

  def main(args: Array[String]) {
    val futureEmail = new Email(List("Feel free to reply if you have any ideas."), "unknown")
    val futureModel = new EmailModel
    for { word <- featureWords } {
      futureModel.hasWord(word).observe(futureEmail.text.contains(word))
    }

    val alg = MetropolisHastings(1000000, ProposalScheme.default, 100000, futureModel.isSpam)
    alg.start()
    val result = alg.probability(futureModel.isSpam, true)
    println("Probability new email is spam = " + result)
    alg.kill()
  }
}
