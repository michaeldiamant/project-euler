package projecteuler

import org.scalacheck.{Gen, Prop}
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class P004Spec extends Specification with ScalaCheck {

  "Oracle and non-oracle isPalindrome functions" should {
    "yield the same result" >> {
      Prop.forAllNoShrink(Gen.choose(10, 1e7.toInt)) {
        n => P004.isPalindromeOracle(n) ==== P004.isPalindrome(n)
      }
    }
  }

  "The largest palindrome resulting from the product of two three digit " +
    "numbers" should {
    "be 906609" >> {
      val (_, _, result) = P004.findLargestPalindromeFromNDigitProduct(3)
      result ==== 906609
    }
  }
}
