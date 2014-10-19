package projecteuler

// If we list all the natural numbers below 10 that are multiples of 3 or 5,
// we get 3, 5, 6 and 9. The sum of these multiples is 23.
//
// Find the sum of all the multiples of 3 or 5 below 1000.
object P001 extends App {

  def multiples(n: Int, max: Int): List[Int] =
    (
      for {
        i <- n to (max - 1)
        if i % n == 0
      } yield
        i)
      .toList

  def sumOfMultiples(ns: List[Int], max: Int): Int =
    multiples(n = 3, max = max)
      .union(multiples(n = 5, max = max))
      .distinct
      .sum

  println(sumOfMultiples(3 :: 5 :: Nil, 1000))
}
