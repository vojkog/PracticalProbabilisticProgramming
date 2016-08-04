package chap13

import com.cra.figaro.algorithm.learning.EMWithVE
import com.cra.figaro.patterns.learning.{ModelParameters, ParameterCollection}
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Dirichlet
import com.cra.figaro.algorithm.factored.VariableElimination

object OnlineEM {
  def main(args: Array[String]) {
    val parameters = ModelParameters()
    val d = Dirichlet(2.0,2.0,2.0)("d",parameters)

    class Model(parameters: ParameterCollection, modelUniverse: Universe) {
      val s = Select(parameters.get("d"), 1, 2, 3)("s", modelUniverse)
    }

    def f = () => {
      val modelUniverse = new Universe
      new Model(parameters.priorParameters, modelUniverse)
      modelUniverse
    }

    val em = EMWithVE.online(f, parameters)
    em.start()

    for (i <- 1 to 100) {
      val evidence = List(NamedEvidence("s", Observation(1)))
      em.update(evidence)
    }

    val futureUniverse1 = Universe.createNew()
    val futureModel1 = new Model(parameters.posteriorParameters, futureUniverse1)
    println(VariableElimination.probability(futureModel1.s, 1))

    for (i <- 101 to 200) {
      val evidence = List(NamedEvidence("s", Observation(2)))
      em.update(evidence)
    }

    val futureUniverse2 = Universe.createNew()
    val futureModel2 = new Model(parameters.posteriorParameters, futureUniverse2)
    println(VariableElimination.probability(futureModel2.s, 1))
  }
}
