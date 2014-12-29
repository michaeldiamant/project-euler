package projecteuler

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{OutputTimeUnit, BenchmarkMode, Mode, Benchmark}

import scala.annotation.tailrec

// A palindromic number reads the same both ways. The largest palindrome made
// from the product of two 2-digit numbers is 9009 = 91 × 99.
//
// Find the largest palindrome made from the product of two 3-digit numbers.
object P004 extends App {

  object PalindromeImplementationExperiments {

    def isPalindromeUsingStringCompare(n: Int): Boolean = {
      val s = n.toString
      s == s.reverse
    }

    def isPalindromeComparingDigitsUsingList(n: Int): Boolean = {
      @tailrec
      def extractDigits(n: Int, digits: List[Int]): List[Int] =
        n < 10 match {
          case true => n :: digits
          case false => extractDigits(n / 10, n % 10 :: digits)
        }

      val digits = extractDigits(n, Nil)
      val half = digits.size / 2
      digits.drop(half).reverse == digits.dropRight(half)
    }

    def isPalindromeComparingDigitsUsingVector(n: Int): Boolean = {
      @tailrec
      def extractDigits(n: Int, digits: Vector[Int]): Vector[Int] =
        n < 10 match {
          case true => digits :+ n
          case false => extractDigits(n / 10, digits :+ n % 10)
        }

      val digits = extractDigits(n, Vector.empty)
      val half = digits.size / 2
      digits.drop(half).reverse == digits.dropRight(half)
    }

    def isPalindromeComputingReverseInteger(n: Int): Boolean = {
      @tailrec
      def reverse(n: Int, reversed: Int): Int =
        n < 10 match {
          case true => append(reversed, n)
          case false => reverse(n / 10, append(reversed, n))
        }

      def append(x: Int, y: Int): Int = x * 10 + y % 10

      n == reverse(n, 0)
    }

    def isPalindromeComputingReverseIntegerWithInlineHint(n: Int): Boolean = {
      @tailrec
      def reverse(n: Int, reversed: Int): Int =
        n < 10 match {
          case true => append(reversed, n)
          case false => reverse(n / 10, append(reversed, n))
        }

      @inline
      def append(x: Int, y: Int): Int = x * 10 + y % 10

      n == reverse(n, 0)
    }
  }

  import PalindromeImplementationExperiments._

  // Run benchmarks in sbt:
  // run -i 5 -r 10s -wi 5 -w 5s -f1 \
  // -jvmArgs "-server -Xms512m -Xmx512m -XX:+UseConcMarkSweepGC' .*PalindromeBenchmarks.*"
  class PalindromeBenchmarks {

    private def rangeBenchmark(f: Int => Boolean): Seq[Boolean] =
      for (n <- 100 to 1e7.toInt) yield
        f(n)

    @Benchmark
    @BenchmarkMode(Array(Mode.SampleTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def benchmarkIsPalindromeUsingStringCompare: Seq[Boolean] =
      rangeBenchmark(isPalindromeUsingStringCompare)

    @Benchmark
    @BenchmarkMode(Array(Mode.SampleTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def benchmarkIsPalindromeComparingDigitsAccumulatedInList: Seq[Boolean] =
      rangeBenchmark(isPalindromeComparingDigitsUsingList)

    @Benchmark
    @BenchmarkMode(Array(Mode.SampleTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def benchmarkIsPalindromeComparingDigitsUsingVector: Seq[Boolean] =
      rangeBenchmark(isPalindromeComparingDigitsUsingVector)

    @Benchmark
    @BenchmarkMode(Array(Mode.SampleTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def benchmarkIsPalindromeComputingReverseInteger: Seq[Boolean] =
      rangeBenchmark(isPalindromeComputingReverseInteger)

    @Benchmark
    @BenchmarkMode(Array(Mode.SampleTime))
    @OutputTimeUnit(TimeUnit.MILLISECONDS)
    def benchmarkIsPalindromeComputingReverseIntegerWithInlineHint: Seq[Boolean] =
      rangeBenchmark(isPalindromeComputingReverseIntegerWithInlineHint)
  }

  def isPalindromeOracle(n: Int): Boolean = isPalindromeUsingStringCompare(n)

  def isPalindrome(n: Int): Boolean =
    isPalindromeComputingReverseIntegerWithInlineHint(n)

  def findLargestPalindromeFromNDigitProduct(digits: Int): (Int, Int, Int) = {
    val max = Math.pow(10, digits).toInt - 1
    val min = Math.pow(10, digits - 1).toInt

    @tailrec
    def findPalindromes(xs: List[Int],
                        ys: List[Int],
                        acc: List[(Int, Int, Int)]): List[(Int, Int, Int)] = {
      (xs, ys) match {
        case (Nil, _) => acc
        case (h :: t, Nil) =>
          findPalindromes(t, h.to(end = min, step = -1).toList, acc)
        case (hx :: _, hy :: ty) =>
          val product = hx * hy
          isPalindrome(product) match {
            case true => findPalindromes(xs, ty, (hx, hy, product) :: acc)
            case false => findPalindromes(xs, ty, acc)
          }
      }
    }

    val xs = max.to(end = min, step = -1).toList

    findPalindromes(xs, xs, Nil)
      .sortWith((x, y) => x._3 > y._3)
      .headOption
      .getOrElse((0, 0, 0))
  }

  println(findLargestPalindromeFromNDigitProduct(3))
}
