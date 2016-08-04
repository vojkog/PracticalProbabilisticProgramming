package chap05

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete.{Binomial, Poisson}
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.sampling.Importance

object ProductDistribution {
  class Network(popularity: Double) {
    val numNodes = Poisson(popularity)
  }

  class Model(targetPopularity: Double, productQuality: Double, affordability: Double) {
    def generateLikes(numFriends: Int, productQuality: Double): Element[Int] = {
      def helper(friendsVisited: Int, totalLikes: Int, unprocessedLikes: Int): Element[Int] = {
        if (unprocessedLikes == 0) Constant(totalLikes)
        else {
          val unvisitedFraction = //#C
            1.0 - (friendsVisited.toDouble - 1)/ (numFriends - 1) //#C
          val newlyVisited = Binomial(2, unvisitedFraction)
          val newlyLikes = Binomial(newlyVisited, Constant(productQuality))
          Chain(newlyVisited, newlyLikes,
                (visited: Int, likes: Int) =>
                  helper(friendsVisited + visited, totalLikes + likes, unprocessedLikes + likes - 1))
        }
      }
      helper(1, 1, 1)
    }

    val targetSocialNetwork = new Network(targetPopularity)
    val targetLikes = Flip(productQuality)
    val numberFriendsLike =
      Chain(targetLikes, targetSocialNetwork.numNodes,
            (l: Boolean, n: Int) =>
              if (l) generateLikes(n, productQuality); else Constant(0))
    val numberBuy = Binomial(numberFriendsLike, Constant(affordability))
  }

  def predict(targetPopularity: Double, productQuality: Double, affordability: Double): Double = {
    val model = new Model(targetPopularity, productQuality, affordability)
    val algorithm = Importance(1000, model.numberBuy)
    algorithm.start()
    val result = algorithm.expectation(model.numberBuy, (i: Int) => i.toDouble)
    algorithm.kill()
    result
  }

  def main(args: Array[String]) {
    println("Popularity\tProduct quality\tAffordability\tPredicted number of buyers")
    println("100       \t0.5            \t0.5          \t" + predict(100, 0.5, 0.5))
    println("100       \t0.5            \t0.9          \t" + predict(100, 0.5, 0.9))
    println("100       \t0.9            \t0.5          \t" + predict(100, 0.9, 0.5))
    println("100       \t0.9            \t0.9          \t" + predict(100, 0.9, 0.9))
    println("10        \t0.5            \t0.5          \t" + predict(10, 0.5, 0.5))
    println("10        \t0.5            \t0.9          \t" + predict(10, 0.5, 0.9))
    println("10        \t0.9            \t0.5          \t" + predict(10, 0.9, 0.5))
    println("10        \t0.9            \t0.9          \t" + predict(10, 0.9, 0.9))
  }
}
