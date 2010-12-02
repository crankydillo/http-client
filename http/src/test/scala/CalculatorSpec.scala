package org.beeherd.http

import org.specs._
import org.specs.runner.JUnit4
        
class CalculatorSpecTest extends JUnit4(CalculatorSpec)
object CalculatorSpec extends Specification {
  private val calculator = new Calculator

  "A Calculator" should {
    "compute the sum of 2 integers" in {
      calculator.add(1, 1) must beEqual(2)
    }

    "return the history of operations as a list of strings" in {
      val calc = new Calculator
      calc.add(1, 1)
      calc.add(2, 2)

      val history = calc.history

      history must haveSize(2)
      history must containInOrder(List("2 + 2", "1 + 1"))
    }
  }
}
