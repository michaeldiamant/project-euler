package projecteuler

import scala.annotation.tailrec

// The prime factors of 13195 are 5, 7, 13 and 29.
//
// What is the largest prime factor of the number 600851475143 ?
object P003 extends App {

  // Stream-based trial division approach only suitable for small n.  Large
  // values of n cause StackOverflowError
  object TrialDivision {
    def from(start: Long): Stream[Long] =
      start #:: from(start + 1)

    def primes(s: Stream[Long]): Stream[Long] =
      s.head #:: primes(s.tail.filter(p => p % s.head != 0))

    def largestPrimeFactorOf(n: Long): Long =
      primes(from(2))
        .takeWhile(_ < n)
        .reverse
        .find(n % _ == 0)
        .getOrElse(n)
  }


  object PrimeFactorization {

    def naiveLargestPrimeFactorOf(n: Long): Long = {
      @tailrec
      def doLargestPrimeFactorOf(x: Long, factor: Long): Long = {
        x > factor match {
          case true =>
            x % factor == 0 match {
              case true =>
                doLargestPrimeFactorOf(x / factor, factor)
              case false =>
                doLargestPrimeFactorOf(x, factor + 1)
            }
          case false => factor
        }
      }

      doLargestPrimeFactorOf(n, 2)
    }

    // Solution optimizes runtime by minimizing the number of factors it
    // evaluates by taking advantage of the knowledge that for the number n,
    // there can only be at most, one prime factor larger than sqrt(n).  See
    // Project Euler solution doc for more information.
    def optimizedLargestPrimeFactorOf(n: Long): Long = {
      @tailrec
      def doLargestPrimeFactorOf(x: Long, factor: Long): Long = {
        Math.sqrt(x).toLong >= factor match {
          case true =>
            x % factor == 0 match {
              case true =>
                doLargestPrimeFactorOf(x / factor, factor)
              case false =>
                doLargestPrimeFactorOf(x, factor + 1)
            }
          case false => x
        }
      }

      doLargestPrimeFactorOf(n, 2)
    }

  }

  println(PrimeFactorization.optimizedLargestPrimeFactorOf(600851475143l))
}

