package chap06

import com.cra.figaro.language.{Flip, Constant} //#A
import com.cra.figaro.library.atomic.continuous.{Uniform, Beta} //#A
import com.cra.figaro.library.compound.If //#A
import com.cra.figaro.algorithm.sampling.Importance //#A

object Hierarchical {
  def main(args: Array[String]) {
    val numCoins = args.length

    val fairProbability = Uniform(0.0, 1.0)

    val isFair =
      for { coin <- 0 until numCoins }
      yield Flip(fairProbability)
    val biases =
      for { coin <- 0 until numCoins }
      yield If(isFair(coin), Constant(0.5), Beta(2,5))

    val tosses =
      for { coin <- 0 until numCoins }
      yield {
        for { toss <- 0 until args(coin).length }
        yield Flip(biases(coin))
      }

    for {
      coin <- 0 until numCoins
      toss <- 0 until args(coin).length
    } {
      val outcome = args(coin)(toss) == 'H'
      tosses(coin)(toss).observe(outcome)
    }

    val algorithm = Importance(fairProbability, biases(0))
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    val averageFairProbability = algorithm.mean(fairProbability)
    val firstCoinAverageBias = algorithm.mean(biases(0))
    println("Average fairness probability: " + averageFairProbability)
    println("First coin average bias: " + firstCoinAverageBias)

    algorithm.kill()
  }
}
