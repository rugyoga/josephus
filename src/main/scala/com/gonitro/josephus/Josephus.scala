package com.gonitro.josephus

import collection.immutable.Queue

object Josephus {
  def solve(ns: Queue[Int], k: Int, take: Int): Int =
    if (ns.length == 1)
      ns.head
    else if (take == 0)
      solve(ns.tail, k, k-1)
    else if (take < ns.length)
      ns.splitAt(take) match { case (a, b) => solve(b.enqueue(a), k, 0) }
    else
      solve(ns, k, take % ns.length)

  def solve(n: Int, k: Int): Int = {
    require(n > 0)
    require(k > 0)
    solve(Queue(1 to n: _*), k, k-1)
  }

  def main(args: Array[String]): Unit =
    args match {
      case Array(n, k) => println(solve(n.toInt, k.toInt))
      case _ => println("Usage: Josephus <n> <k>")
    }
}
