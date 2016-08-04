package chap06

import com.cra.figaro.language._
import com.cra.figaro.library.collection.Process
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{^^, If}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.If

object HealthProcess extends Process[Double, Boolean] {
  val healthyPrior = Uniform((0.05 to 0.95 by 0.1):_*)
  val healthChangeRate = Uniform((0.001 to 0.1 by 0.002):_*)

  def generate(time: Double): Element[Boolean] = Flip(healthyPrior)

  def generate(times: List[Double]): Map[Double, Element[Boolean]] = {
    val sortedTimes = times.sorted
    val healthy = sortedTimes.map(time => (time, generate(time))).toMap
    def makePairs(remaining: List[Double]) {
      if (remaining.length >= 2) {
        val time1 :: time2 :: rest = remaining
        val probChange = Apply(healthChangeRate, (d: Double) => 1 - math.exp(- (time2 - time1) / d))
        val equalHealth = healthy(time1) === healthy(time2)
        val healthStatusChecker = If(equalHealth, Constant(true), Flip(probChange))
        healthStatusChecker.observe(true)
        makePairs(time2 :: rest)
      }
    }
    makePairs(sortedTimes)
    healthy
  }

  def rangeCheck(time: Double) = time >= 0

  def main(args: Array[String]) {
    val data = Map(0.1 -> true, 0.25 -> true, 0.3 -> false, 0.31 -> false, 0.34 -> false, 0.36 -> false, 0.4 -> true, 0.5 -> true, 0.55 -> true)
    val queries = List(0.35, 0.37, 0.45, 0.6)
    val targets = queries ::: data.keys.toList

    val healthy = generate(targets)
    for { (time, value) <- data } {
      healthy(time).observe(value)
    }

    val queryElements = queries.map(healthy(_))
    val queryTargets = healthyPrior :: healthChangeRate :: queryElements
    val algorithm = VariableElimination(queryTargets:_*)
    algorithm.start()
    for { query <- queries } {
      println("Probability the patient is healthy at time " + query + " = " + algorithm.probability(healthy(query), true))
    }
    println("Expected prior probability of healthy = " + algorithm.mean(healthyPrior))
    println("Expected health change rate = " + algorithm.mean(healthChangeRate))
    algorithm.kill()
  }
}
