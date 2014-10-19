package projecteuler

import org.specs2.mutable.Specification

class P001Spec extends Specification {

  "Multiples of 3 below 10" should {

    "be 3, 6, 9" >> {
      P001.multiples(3, 10) ==== 3 :: 6 :: 9 :: Nil
    }
  }

  "Multiples of 3 or 5 for natural numbers below 10" should {

    "sum to 23" >> {
      P001.sumOfMultiples(3 :: 5 :: Nil, 10) ==== 23
    }
  }
}
