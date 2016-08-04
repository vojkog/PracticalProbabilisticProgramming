package chap11

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._

object ActorsAndMovies {
  val random = new scala.util.Random()

  class Actor {
    val famous = Flip(0.1)
  }

  class Movie {
    val quality = Select(0.3 -> 'low, 0.5 -> 'medium, 0.2 -> 'high)
  }

  class Appearance(val actor: Actor, val movie: Movie) {
    def getProb(quality: Symbol, famous: Boolean) =
      (quality, famous) match {
        case ('low, false) => 0.001
        case ('low, true) => 0.01
        case ('medium, false) => 0.01
        case ('medium, true) => 0.05
        case ('high, false) => 0.05
        case ('high, true) => 0.2
      }
    val probability =
      Apply(movie.quality, actor.famous, (q: Symbol, f: Boolean) => getProb(q, f))
    val award: Element[Boolean] =
      Flip(probability)
  }

  val numActors = 200
  val numMovies = 100
  val numAppearances = 300

  val actors = Array.fill(numActors)(new Actor)
  val movies = Array.fill(numMovies)(new Movie)
  val appearances = Array.fill(numAppearances)(new Appearance(actors(random.nextInt(numActors)), movies(random.nextInt(numMovies))))

  // Ensure that exactly one appearance gets an award.
  def uniqueAwardConstraint(awards: List[Boolean]) = {
    val n = awards.count(b => b)
    if (n == 0) 0.0 else 1.0 / (n * n)
  }
  val allAwards: Element[List[Boolean]] = Inject(appearances.map(_.award): _*)
  allAwards.setConstraint(uniqueAwardConstraint)

  val scheme: ProposalScheme = {
    DisjointScheme(
      (0.5, () => ProposalScheme(appearances(random.nextInt(numAppearances)).award, appearances(random.nextInt(numAppearances)).award)),
      (0.25, () => ProposalScheme(actors(random.nextInt(numActors)).famous)),
      (0.25, () => ProposalScheme(movies(random.nextInt(numMovies)).quality)))
  }

  def main(args: Array[String]): Unit = {
    appearances(0).actor.famous.observe(true)
    appearances(0).movie.quality.observe('high)

    val alg = MetropolisHastings(1000000, scheme, 100000, appearances(0).award)
    alg.start()
    println("Probability the first appearance gets an award, given that the actor is famous and the movie is high quality = " +
            alg.probability(appearances(0).award, true))
    alg.kill()
  }
}
