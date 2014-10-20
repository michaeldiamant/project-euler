package projecteuler

import org.scalacheck.{Prop, Gen}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class P001Spec extends Specification with ScalaCheck {

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

  "Alternative implementations" should {

    def quotient: Gen[Int] = Gen.choose(1, 1000)

    def quotients: Gen[List[Int]] =
      for {
        n <- Gen.choose(1, 100)
        xs <- Gen.listOfN(n = n, quotient)
      } yield
        xs

    def max: Gen[Int] = Gen.choose(1, 1000)

    "produce the same result as the original implementation" >> {
      Prop.forAllNoShrink(quotients, max) {
        (quotients, max) =>
          P001.sumOfMultiples(quotients, max) ====
            P001.sumOfMultiplesFaster(quotients, max)

          P001.sumOfMultiples(quotients, max) ====
            P001.sumOfMultiplesLazily(quotients, max)
      }
    }
  }
}
