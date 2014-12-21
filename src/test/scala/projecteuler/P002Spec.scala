package projecteuler

import org.scalacheck.{Prop, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class P002Spec extends Specification with ScalaCheck {

  "Fibonacci sequence with numbers < 90" should {
    "be 1, 2, 3, 5, 8, 13, 21, 34, 55, 89" >> {
      P002.fibonacciUntil(max = 90) ====
        1 :: 2 :: 3 :: 5 :: 8 :: 13 :: 21 :: 34 :: 55 :: 89 :: Nil
    }

  }

  "Eager and lazy implementations" should {
    "yield the same result" >> {
      def max = Gen.choose(1, 10000)
      Prop.forAllNoShrink(max) {
        max =>
          P002.sumEvenValuedFibonacciUntil(max) ====
            P002.lazilySumEvenValuedFibonacciUntil(max)
      }
    }
  }
}
