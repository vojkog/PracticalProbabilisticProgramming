package chap11

import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.compound.^^
import com.cra.figaro.algorithm.sampling.MetropolisHastings

object TwoVariableMCMC {
  def main(args: Array[String]) {
    val x = Normal(0.75, 0.2)
    val y = Normal(0.4, 0.2)
    x.setCondition((d: Double) => d > 0 && d < 1)
    y.setCondition((d: Double) => d > 0 && d < 1)
    val pair = ^^(x, y)
    println(MetropolisHastings.probability(pair, (xy: (Double, Double)) => xy._1 > 0.5 && xy._2 > 0.5))
  }
}
