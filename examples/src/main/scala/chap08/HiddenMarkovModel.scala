package chap08

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Beta
import com.cra.figaro.library.compound.If
import com.cra.figaro.algorithm.factored.VariableElimination

object HiddenMarkovModel {
  val length = 90
  val confident: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  val ourPossession: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
  confident(0) = Flip(0.4)
  for { minute <- 1 until length } {
    confident(minute) = If(confident(minute - 1), Flip(0.6), Flip(0.3))
  }
  for { minute <- 0 until length } {
    ourPossession(minute) = If(confident(minute), Flip(0.7), Flip(0.3))
  }

  def main(args: Array[String]) {
    println("Probability we are confident at time step 2")
    println("Prior probability: " + VariableElimination.probability(confident(2), true))
    ourPossession(2).observe(true)
    println("After observing current possession at time step 2: " + VariableElimination.probability(confident(2), true))
    ourPossession(1).observe(true)
    println("After observing previous possession at time step 1: " + VariableElimination.probability(confident(2), true))
    ourPossession(0).observe(true)
    println("After observing previous possession at time step 0: " + VariableElimination.probability(confident(2), true))
    ourPossession(3).observe(true)
    println("After observing future possession at time step 3: " + VariableElimination.probability(confident(2), true))
    ourPossession(4).observe(true)
    println("After observing future possession at time step 4: " + VariableElimination.probability(confident(2), true))
  }
}
