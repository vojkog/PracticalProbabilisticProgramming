package chap05

import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.factored.VariableElimination

object PrinterProblem {
    val printerPowerButtonOn = Flip(0.95)
    val tonerLevel = Select(0.7 -> 'high, 0.2 -> 'low, 0.1 -> 'out)
    val tonerLowIndicatorOn =
      If(printerPowerButtonOn,
         CPD(tonerLevel,
             'high -> Flip(0.2),
             'low -> Flip(0.6),
             'out -> Flip(0.99)),
         Constant(false))
    val paperFlow = Select(0.6 -> 'smooth, 0.2 -> 'uneven, 0.2 -> 'jammed)
    val paperJamIndicatorOn =
      If(printerPowerButtonOn,
         CPD(paperFlow,
             'smooth -> Flip(0.1),
             'uneven -> Flip(0.3),
             'jammed -> Flip(0.99)),
         Constant(false))
    val printerState =
      Apply(printerPowerButtonOn, tonerLevel, paperFlow,
            (power: Boolean, toner: Symbol, paper: Symbol) => {
              if (power) {
                if (toner == 'high && paper == 'smooth) 'good
                else if (toner == 'out || paper == 'out) 'out
                else 'poor
              } else 'out
            })
    val softwareState = Select(0.8 -> 'correct, 0.15 -> 'glitchy, 0.05 -> 'crashed)
    val networkState = Select(0.7 -> 'up, 0.2 -> 'intermittent, 0.1 -> 'down)
    val userCommandCorrect = Flip(0.65)
    val numPrintedPages =
      RichCPD(userCommandCorrect, networkState, softwareState, printerState,
          (*, *, *, OneOf('out)) -> Constant('zero),
          (*, *, OneOf('crashed), *) -> Constant('zero),
          (*, OneOf('down), *, *) -> Constant('zero),
          (OneOf(false), *, *, *) -> Select(0.3 -> 'zero, 0.6 -> 'some, 0.1 -> 'all),
          (OneOf(true), *, *, *) -> Select(0.01 -> 'zero, 0.01 -> 'some, 0.98 -> 'all))
    val printsQuickly =
      Chain(networkState, softwareState,
            (network: Symbol, software: Symbol) =>
              if (network == 'down || software == 'crashed) Constant(false)
              else if (network == 'intermittent || software == 'glitchy) Flip(0.5)
              else Flip(0.9))
    val goodPrintQuality =
      CPD(printerState,
          'good -> Flip(0.95),
          'poor -> Flip(0.3),
          'out -> Constant(false))
    val printResultSummary =
      Apply(numPrintedPages, printsQuickly, goodPrintQuality,
            (pages: Symbol, quickly: Boolean, quality: Boolean) =>
            if (pages == 'zero) 'none
            else if (pages == 'some || !quickly || !quality) 'poor
            else 'excellent)

  def step1() {
    val answerWithNoEvidence = VariableElimination.probability(printerPowerButtonOn, true)
    println("Prior probability the printer power button is on = " + answerWithNoEvidence)
  }

  def step2() {
    printResultSummary.observe('poor)
    val answerIfPrintResultPoor = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given a poor result = " + answerIfPrintResultPoor)
  }

  def step3() {
    printResultSummary.observe('none)
    val answerIfPrintResultNone = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given empty result = " + answerIfPrintResultNone)
  }

  def step4() {
    printResultSummary.unobserve()
    printerState.observe('out)
    val answerIfPrinterStateOut = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given " + "out printer state = " + answerIfPrinterStateOut)

    printResultSummary.observe('none)
    val answerIfPrinterStateOutAndResultNone = VariableElimination.probability(printerPowerButtonOn, true)
    println("Probability the printer power button is on given out printer state and empty result = " + answerIfPrinterStateOutAndResultNone)
  }

  def step5() {
    printResultSummary.unobserve()
    printerState.unobserve()
    val printerStateGoodPrior = VariableElimination.probability(printerState, 'good)
    println("Prior probability the printer state is good = " + printerStateGoodPrior)

    tonerLowIndicatorOn.observe(true)
    val printerStateGoodGivenTonerLowIndicatorOn = VariableElimination.probability(printerState, 'good)
    println("Probability printer state is good given low toner indicator = " + printerStateGoodGivenTonerLowIndicatorOn)
  }

  def step6() {
    tonerLowIndicatorOn.unobserve()
    val softwareStateCorrectPrior = VariableElimination.probability(softwareState, 'correct)
    println("Prior probability the software state is correct = " + softwareStateCorrectPrior)

    networkState.observe('up)
    val softwareStateCorrectGivenNetworkUp = VariableElimination.probability(softwareState, 'correct)
    println("Probability software state is correct given network up = " + softwareStateCorrectGivenNetworkUp)
  }

  def step7() {
    networkState.unobserve()
    printsQuickly.observe(false)
    val softwareStateCorrectGivenPrintsSlowly = VariableElimination.probability(softwareState, 'correct)
    println("Probability software state is correct given prints slowly = " + softwareStateCorrectGivenPrintsSlowly)

    networkState.observe('up)
    val softwareStateCorrectGivenPrintsSlowlyAndNetworkUp = VariableElimination.probability(softwareState, 'correct)
    println("Probability software state is correct given prints slowly and network up = " + softwareStateCorrectGivenPrintsSlowlyAndNetworkUp)
  }

  def main(args: Array[String]) {
    step1()
    step2()
    step3()
    step4()
    step5()
    step6()
    step7()
  }
}
