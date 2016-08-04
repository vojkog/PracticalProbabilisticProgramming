package chap12

import com.cra.figaro.language.{Element, Flip}
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.collection.Container
import com.cra.figaro.algorithm.sampling.Importance

object JointDistribution {
  val economicClimateGood = Flip(0.5)
  def makeSales: Element[Int] = If(economicClimateGood, Uniform(80, 120, 160), Uniform(60, 90, 120))
  val sales = Array.fill(20)(makeSales)
  val totalSales = Container(sales:_*).reduce(_ + _)

  def main(args: Array[String]) {
    val salesPair = ^^(sales(0), sales(1))
    val ve = VariableElimination(sales(0), sales(1), salesPair)
    ve.start()
    val imp = Importance(10000, totalSales)
    imp.start()
    println("Probability first sales will be less than 100 = " +
      ve.probability(sales(0), (i: Int) => i < 100))
    println("Probability second sales will be less than 100 = " +
      ve.probability(sales(1), (i: Int) => i < 100))
    println("Probability both sales will be less than 100 = " +
      ve.probability(salesPair, (pair: (Int, Int)) => pair._1 < 100 && pair._2 < 100))
    println("Probability total sales are less than 2000 = " +
      imp.probability(totalSales, (i: Int) => i < 2000))
    println("Mean individual sales = " +
      ve.expectation(sales(0), (i: Int) => i.toDouble))
    println("Mean total sales = " +
      imp.expectation(totalSales, (i: Int) => i.toDouble))
    ve.kill()
    imp.kill()
  }
}
