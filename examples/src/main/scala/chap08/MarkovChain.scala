package chap08

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object MarkovChain {
  val length = 90
  val ourPossession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  ourPossession(0) = Flip(0.5)
  for { minute <- 1 until length } {
    ourPossession(minute) = If(ourPossession(minute - 1), Flip(0.6), Flip(0.3))
  }

  def main(args: Array[String]) {
    println("Probability we have possession at time step 5")
    println("Prior probability: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(4).observe(true)
    println("After observing that we have possession at time step 4: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(3).observe(true)
    println("After observing that we have possession at time step 3: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(6).observe(true)
    println("After observing that we have possession at time step 6: " + VariableElimination.probability(ourPossession(5), true))
    ourPossession(7).observe(true)
    println("After observing that we have possession at time step 7: " + VariableElimination.probability(ourPossession(5), true))
  }
}
