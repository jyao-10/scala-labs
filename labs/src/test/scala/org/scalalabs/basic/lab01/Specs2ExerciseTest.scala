package org.scalalabs.basic.lab01

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
/**
 * In this Lab you will implement a Specs2 testcase.
 *
 * Instructions:
 * 1. Implement the divide method in Euro that has the following signature:  def /(divider:Int) = ???
 * - If the divider is <=0 throw an IllegalArgumentException
 *
 * 2. Write a Specs2 specification to test:
 * - Happy flow (divider is > 0)
 * - Alternative flow (divider is <= 0)
 */
@RunWith(classOf[JUnitRunner])
class Specs2ExerciseTest extends Specification {
  "divide in Euro" should {
    "divide correctly if divider > 0" in {
      val e = new Euro(3, 12) / 4
      e.euro ==== 0
      e.cents ==== 78
    }
    "throw IllegalArgumentException if divider <= 0" in {
      new Euro(3, 12) / 0 must throwAn[IllegalArgumentException]
    }
  }
}

