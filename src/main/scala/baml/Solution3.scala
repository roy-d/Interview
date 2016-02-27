package baml

import scala.annotation.tailrec
import scala.collection.JavaConversions._

/**
  * Created by Roy on 2/27/2016.
  */
class Solution3 {

  def solution(A: Array[Int]): Int = {
    helper(A.toList,0)
  }


  @tailrec
  private def helper(days: List[Int], price: Int): Int = days match{
    case Nil => price
    case d::Nil => price + 2
    case d::tail =>
      if(tail.takeWhile(_ < d+30).size >21 ) helper(tail.dropWhile(_ < d+30), price+25)
      else if(tail.takeWhile(_ < d+7).size >2 ) helper(tail.dropWhile( _ < d+7), price+7)
      else helper(tail, price+2)

  }

}
