package com.gonitro.jospehus

import org.scalatest._
import com.gonitro.josephus._

class JosephusSpec extends FlatSpec {
  //https://en.wikipedia.org/wiki/Josephus_problem
  def closed(n: Int, k: Int): Int = if (n == 1) 1 else ((closed(n-1,k) + k -1) % n) + 1

  for (n <- 1 to 128; k <- 1 to 128)
  "Josephus" should s"match the result of closed form solution for n = $n and k = $k" in {
    assert(Josephus.solve(n, k) === closed(n, k))
  }

  "Josephus" should s"match the result of closed form solution for large binary power of 2" in {
    assert(Josephus.solve(1024*1024, 2) === closed(1024*1024, 2))
  }
}
