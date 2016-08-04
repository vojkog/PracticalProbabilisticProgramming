package chap07

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object PrinterProblemInheritance {
  abstract class Printer {
    val powerButtonOn = Flip(0.95)

    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed)
    val paperJamIndicatorOn =
      If(powerButtonOn,
         CPD(paperFlow,
             'smooth -> Flip(0.1),
             'uneven -> Flip(0.3),
             'jammed -> Flip(0.99)),
         Constant(false))

    val state: Element[Symbol]
  }

  class LaserPrinter extends Printer {
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out)
    val tonerLowIndicatorOn =
      If(powerButtonOn,
         CPD(tonerLevel,
             'high -> Flip(0.2),
             'low -> Flip(0.6),
             'out -> Flip(0.99)),
         Constant(false))
    val state =
      Apply(powerButtonOn, tonerLevel, paperFlow,
            (power: Boolean, toner: Symbol, paper: Symbol) => {
              if (power) {
                if (toner == 'high && paper == 'smooth) 'good
                else if (toner == 'out || paper == 'out) 'out
                else 'poor
              } else 'out
            }
      )
  }

  class InkjetPrinter extends Printer {
    val inkCartridgeEmpty = Flip(0.1)
    val inkCartridgeEmptyIndicator = If(inkCartridgeEmpty, Flip(0.99), Flip(0.3))
    val cloggedNozzle = Flip(0.001)
    val state =
      Apply(powerButtonOn, inkCartridgeEmpty, cloggedNozzle, paperFlow,
            (power: Boolean, ink: Boolean, nozzle: Boolean, paper: Symbol) => {
              if (power && !ink && !nozzle) {
                if (paper == 'smooth) 'good
                else if (paper == 'uneven) 'poor
                else 'out
              } else 'out
            }
      )
  }

  class Software {
    val state = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed)
  }

  class Network {
    val state = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down)
  }

  class User {
    val commandCorrect = Flip(0.65)
  }

  class PrintExperience(printer: Printer, software: Software, network: Network, user: User) {
    val numPrintedPages =
      RichCPD(user.commandCorrect, network.state, software.state, printer.state,
          (*, *, *, OneOf('out)) -> Constant('zero),
          (*, *, OneOf('crashed), *) -> Constant('zero),
          (*, OneOf('down), *, *) -> Constant('zero),
          (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
          (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
      Chain(network.state, software.state,
            (network: Symbol, software: Symbol) =>
              if (network == 'down || software == 'crashed) Constant(false)
              else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
              else Flip(0.9))
    val goodPrintQuality =
      CPD(printer.state,
          'good -> Flip(0.95),
          'poor -> Flip(0.3),
          'out -> Constant(false))
    val summary =
      Apply(numPrintedPages, printsQuickly, goodPrintQuality,
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
            if (pages == 'zero) 'none
            else if (pages == 'some || !quickly || !quality) 'poor
            else 'excellent)
  }

  val myLaserPrinter = new LaserPrinter
  val myInkjetPrinter = new InkjetPrinter
  val mySoftware = new Software
  val myNetwork = new Network
  val me = new User
  val myExperience1 = new PrintExperience(myLaserPrinter, mySoftware, myNetwork, me)
  val myExperience2 = new PrintExperience(myInkjetPrinter, mySoftware, myNetwork, me)

  def step1() {
    myExperience1.summary.observe('none)
    val alg = VariableElimination(myLaserPrinter.powerButtonOn, myNetwork.state)
    alg.start()
    println("After observing that printing with the laser printer produces no result:")
    println("Probability laser printer power button is on = " + alg.probability(myLaserPrinter.powerButtonOn, true))
    println("Probability network is down = " + alg.probability(myNetwork.state, 'down))
    alg.kill()
  }

  def step2() {
    myExperience2.summary.observe('none)
    val alg = VariableElimination(myLaserPrinter.powerButtonOn, myNetwork.state)
    alg.start()
    println("\nAfter observing that printing with the inkjet printer also produces no result:")
    println("Probability laser printer power button is on = " + alg.probability(myLaserPrinter.powerButtonOn, true))
    println("Probability network is down = " + alg.probability(myNetwork.state, 'down))
    alg.kill()
  }

  def main(args: Array[String]) {
    step1()
    step2()
  }
}
