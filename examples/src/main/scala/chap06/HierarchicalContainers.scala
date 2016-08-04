package chap06

import com.cra.figaro.language.{Flip, Constant}
import com.cra.figaro.library.atomic.continuous.{Uniform, Beta}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.FixedSizeArray

object HierarchicalContainers {
  def main(args: Array[String]) {
    val numCoins = args.length

    val fairProbability = Uniform(0.0, 1.0)

    val isFair = new FixedSizeArray(numCoins + 1, i => Flip(fairProbability))
    val biases = isFair.chain(if (_) Constant(0.5) else Beta(2,5))

    val tosses =
      for { coin <- 0 until numCoins } yield new FixedSizeArray(args(coin).length, i => Flip(biases(coin)))

    val hasHeads =
      for { coin <- 0 until numCoins } yield tosses(coin).exists(b => b)

    for {
      coin <- 0 until numCoins
      toss <- 0 until args(coin).length
    } {
      args(coin)(toss) match {
        case 'H' => tosses(coin)(toss).observe(true)
        case 'T' => tosses(coin)(toss).observe(false)
        case _ => ()
      }
    }

    val algorithm = Importance(fairProbability, hasHeads(2))
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()
    println("Probability at least one of the tosses of the third coin was heads = " + algorithm.probability(hasHeads(2), true))
    algorithm.kill()
  }
}
