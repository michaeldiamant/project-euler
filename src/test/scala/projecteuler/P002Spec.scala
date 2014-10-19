package projecteuler

import org.specs2.mutable.Specification

class P002Spec extends Specification {

  "Fibonacci sequence with numbers < 90" should {

    "be 1, 2, 3, 5, 8, 13, 21, 34, 55, 89" >> {
      P002.fibonacciUntil(max = 90) ====
        1 :: 2 :: 3 :: 5 :: 8 :: 13 :: 21 :: 34 :: 55 :: 89 :: Nil
    }

  }
}
