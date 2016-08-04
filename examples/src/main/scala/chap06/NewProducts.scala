package chap06

import com.cra.figaro.library.atomic.discrete.Geometric
import com.cra.figaro.library.atomic.continuous.{Beta, Normal}
import com.cra.figaro.library.collection.VariableSizeArray
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.Universe
import com.cra.figaro.language.Flip

object NewProducts {
  def runExperiment(rNDLevel: Double) {
    Universe.createNew()
    val numNewProducts = Geometric(rNDLevel)
    val productQuality = VariableSizeArray(numNewProducts, i => Beta(1, i + 1))
    val productSalesRaw = productQuality.chain(Normal(_, 0.5))
    val productSales = productSalesRaw.map(_.max(0))
    val totalSales = productSales.foldLeft(0.0)(_ + _)
    val algorithm = Importance(totalSales)
    algorithm.start()
    Thread.sleep(5000)
    algorithm.stop()
    println("With R&D at " + rNDLevel + ", expected sales will be " + algorithm.mean(totalSales))
    algorithm.kill()
  }

  def main(args: Array[String]) {
    for { i <- 0.05 to 1.0 by 0.1 } { runExperiment(i) }
  }
}
