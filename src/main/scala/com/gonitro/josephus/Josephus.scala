package com.gonitro.josephus;

import scala.collection.immutable.Queue;

object Josephus {
  def solve(soldiers: Queue[Int], k: Int, skip: Int): Int =
    if (soldiers.length == 1)
      soldiers.front
    else if (skip == 0)
      solve(soldiers.tail, k, k-1)
    else if (skip < soldiers.length)
      solve(soldiers.drop(skip).enqueue(soldiers.take(skip)), k, 0)
    else
      solve(soldiers, k, skip % soldiers.length)

  def solve(n: Int, k: Int): Int = solve(Queue(1 to n: _*), k, k-1)

  def main(args: Array[String]): Unit = {
    require(args.size == 2, "Usage: Josephus <n> <k>")
    val ints = args.map(_.toInt)
    println(solve(ints(0), ints(1)))
  }
}
