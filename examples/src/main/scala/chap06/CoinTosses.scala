package chap06

import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.sampling.Importance

object CoinTosses {
  def main(args: Array[String]) {
    val outcomes = args(0)
    val numTosses = outcomes.length

    val bias = Beta(2,5)
    val tosses = Array.fill(numTosses)(Flip(bias))
    val nextToss = Flip(bias)

    for {
      toss <- 0 until numTosses
    } {
      val outcome = outcomes(toss) == 'H'
      tosses(toss).observe(outcome)
    }

    val algorithm = Importance(nextToss, bias)
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    println("Average bias = " + algorithm.mean(bias))
    println("Probability of heads on next toss = " + algorithm.probability(nextToss, true))

    algorithm.kill()
  }
}
