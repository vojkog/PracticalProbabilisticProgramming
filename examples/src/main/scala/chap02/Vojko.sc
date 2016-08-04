package chap01

import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination

object Vojko {
  val sunnyToday = Flip(0.4)                      //> sunnyToday  : com.cra.figaro.language.AtomicFlip = Flip(0.4)
  println(VariableElimination.probability(sunnyToday, true))
                                                  //> 0.4
}