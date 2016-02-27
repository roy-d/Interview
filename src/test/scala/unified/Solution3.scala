package unified

import scala.annotation.tailrec
import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution3{
  def solution(A: Array[Int]): Boolean = {
    // write your code in Scala 2.10

    @tailrec
    def helper(as: List[Int]): Boolean = as match {
      case Nil => sys.error("size of as should be [1..10]")
      case a::Nil => true
      case a::tail =>
        if(a>tail.head) isSorted(swapHeadWithLastMin(as, as.min))
        else helper(tail)
    }

    @tailrec
    def isSorted(as: List[Int]): Boolean = as match{
      case Nil => sys.error("size of as should be [1..10]")
      case a::Nil => true
      case a::tail => if (a>tail.head) false else isSorted(tail)
    }

    def swapHeadWithLastMin(as: List[Int], min: Int): List[Int] = {
      val lastIndex = as.lastIndexOf(min)
      as.updated(0,min).updated(lastIndex, as.head)
    }

    helper(A.toList)
  }
}