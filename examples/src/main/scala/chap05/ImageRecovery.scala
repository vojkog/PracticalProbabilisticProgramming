package chap05

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.beliefpropagation._

object ImageRecovery {
  val pixels = Array.fill(10, 10)(Flip(0.4))
  def setConstraint(i1: Int, j1: Int, i2: Int, j2: Int) {
    val pixel1 = pixels(i1)(j1)
    val pixel2 = pixels(i2)(j2)
    val pair = ^^(pixel1, pixel2)
    pair.addConstraint(bb => if (bb._1 == bb._2) 0.9; else 0.1)
  }
  for {
    i <- 0 until 10
    j <- 0 until 10
  } {
    if (i <= 8) setConstraint(i, j, i+1, j)
    if (j <= 8) setConstraint(i, j, i, j+1)
  }

  def setEvidence(data: String) = {
    for { n <- 0 until data.length } {
      val i = n / 10
      val j = n % 10
      data(n) match {
        case '0' => pixels(i)(j).observe(false)
        case '1' => pixels(i)(j).observe(true)
        case _ => ()
      }
    }
  }

  def main(args: Array[String]) {
  val data =
    """00?000?000
       0?010?0010
       110?010011
       11??000111
       11011000?1
       1?0?100?10
       00001?0?00
       0010??0100
       01?01001?0
       0??000110?""".filterNot(_.isWhitespace)

    setEvidence(data)
    val algorithm = MPEBeliefPropagation(10)
    algorithm.start()
    for {
      i <- 0 until 10
    } {
      for { j <- 0 until 10 } {
        val mlv = algorithm.mostLikelyValue(pixels(i)(j))
        if (mlv) print('1') else print('0')
      }
      println()
    }
  }
}
