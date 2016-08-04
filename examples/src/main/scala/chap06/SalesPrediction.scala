package chap06

import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.language.Flip
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.collection.Container
import com.cra.figaro.library.atomic.discrete.Poisson
import com.cra.figaro.language.Chain

object SalesPrediction {

  def main(args: Array[String]) {
    val numProducts = args.length
    val numRegions = args(0).length

    /* The model */
    val productQuality = Array.fill(numProducts)(Beta(2,2))
    val regionPenetration = Array.fill(numRegions)(Beta(2,2))
    def makeSales(i: Int, j: Int) = Flip(productQuality(i) * regionPenetration(j))
    val highSalesLastYear = Array.tabulate(numProducts, numRegions)(makeSales _)
    val highSalesNextYear = Array.tabulate(numProducts, numRegions)(makeSales _)

    def getSalesByProduct(i: Int) =
      for { j <- 0 until numRegions } yield highSalesNextYear(i)(j)
    val salesPredictionByProduct =
      Array.tabulate(numProducts)(i => Container(getSalesByProduct(i):_*))

    val numHighSales =
      for { predictions <- salesPredictionByProduct }
      yield predictions.count(b => b)

    val numHiresByProduct =
//      for { i <- 0 until numProducts }
//      yield Chain(numHighSales(i), (n: Int) => Poisson(n  + 1))
      Container(numHighSales:_*).chain((n: Int) => Poisson(n + 1))

    /* Observe all the sales */
    for {
      i <- 0 until numProducts
      j <- 0 until numRegions
    } {
      val observation = args(i)(j) == 'T'
      highSalesLastYear(i)(j).observe(observation)
    }

    /* Run inference */
    val targets = numHiresByProduct.elements
    val algorithm = Importance(targets:_*)
    algorithm.start()
    Thread.sleep(10000)
    algorithm.stop()

    /* Report the results */
    for { i <- 0 until numProducts } {
      println("Number of hires for product " + i + ": " + algorithm.expectation(numHiresByProduct(i), (n: Int) => n.toDouble))
    }
    algorithm.kill()
  }
}
