package chap03

import com.cra.figaro.language.Universe

object Evaluator {
  def main(args: Array[String]) = {
    val testDirectoryName = args(0)
    val labelFileName = args(1)
    val kbFileName = args(2)
    val threshold = args(3).toDouble

    val emails = LearningComponent.readEmails(testDirectoryName)
    val labels = LearningComponent.readLabels(labelFileName)
    val (dictionary, learningResults) = ReasoningComponent.loadResults(kbFileName)

    var truePositives = 0
    var falseNegatives = 0
    var falsePositives = 0
    var trueNegatives = 0

    for { (fileName, email) <- emails } {
      println(fileName)
      Universe.createNew()
      val isSpamProbability = ReasoningComponent.classify(dictionary, learningResults, testDirectoryName + "/" + fileName)
      val prediction = isSpamProbability >= threshold
      (labels.get(fileName), prediction) match {
        case (Some(true), true) => truePositives += 1
        case (Some(true), false) => falseNegatives += 1
        case (Some(false), true) => falsePositives += 1
        case (Some(false), false) => trueNegatives += 1
        case _ => ()
      }
    }

    val accuracy = (truePositives + trueNegatives).toDouble / (truePositives + falseNegatives + falsePositives + trueNegatives)
    val precision = truePositives.toDouble / (truePositives + falsePositives)
    val recall = truePositives.toDouble / (truePositives + falseNegatives)

    println("True positives: " + truePositives)
    println("False negatives: " + falseNegatives)
    println("False positives: " + falsePositives)
    println("True negatives: " + trueNegatives)
    println("Threshold: " + threshold)
    println("Accuracy: " + accuracy)
    println("Precision: " + precision)
    println("Recall: " + recall)
  }
}
