package com.gonitro.jospehus

import org.scalatest._
import com.gonitro.josephus._

class JosephusSpec extends FlatSpec with Matchers {
  "Josephus" should "match the result of closed form solution" in withNandK { (n, k) =>
    Josephus.solve(n, k) shouldEqual closed(n, k)
  }

  "Josephus" should "match the result of closed form solution for large binary power of 2" in {
    Josephus.solve(scala.math.pow(2, 13).toInt, 2) shouldEqual 1
  }

  "Josephus" should "throw an IllegalArgumentException for negative n" in {
    an [IllegalArgumentException] should be thrownBy(Josephus.solve(-1, 2))
  }

  def closed(n: Int, k: Int): Int = if (n == 1) 1 else ((closed(n-1,k) + k -1) % n) + 1

  def withNandK(testCode: (Int, Int) => Any): Unit =
    for (n <- 1 to 64; k <- 1 to 64)
      testCode(n, k)
}
