package chap06

import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.sampling.Importance

object Sales {

  def main(args: Array[String]) {
    val numProducts = args.length
    val numRegions = args(0).length

    /* The model */
    val productQuality = Array.fill(numProducts)(Beta(2,2))
    val regionPenetration = Array.fill(numRegions)(Beta(2,2))
    def makeSales(i: Int, j: Int) = Flip(productQuality(i) * regionPenetration(j))
    val highSales = Array.tabulate(numProducts, numRegions)(makeSales _)

    /* Observe all the sales */
    for {
      i <- 0 until numProducts
      j <- 0 until numRegions
    } {
      val observation = args(i)(j) == 'T'
      highSales(i)(j).observe(observation)
    }

    /* Run inference */
    val targets = productQuality ++ regionPenetration
    val algorithm = Importance(targets:_*)
    algorithm.start()
    Thread.sleep(1000)
    algorithm.stop()

    /* Report the results */
    for { i <- 0 until numProducts } {
      println("Product " + i + " quality: " + algorithm.mean(productQuality(i)))
    }
    for { j <- 0 until numRegions } {
      println("Region " + j + " penetration: " + algorithm.mean(regionPenetration(j)))
    }
    algorithm.kill()
  }
}
