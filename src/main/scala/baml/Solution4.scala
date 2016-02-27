package baml

import scala.annotation.tailrec
import scala.collection.JavaConversions._

/**
  * Created by Roy on 2/27/2016.
  */
class Solution4 {

  def solution(T: Array[Int]): Array[Int] = {
    // write your code in Scala 2.10
    val tree = parse(T.toList, List.empty, T.size)
    tree.tail.map(_.size).toArray
  }

  case class Node(id: Int, children: Option[List[Node]])

  def findRoot(as: List[Int]): Int = as.zipWithIndex.filter { case (a, index) => index == a }.head._1

  def findChildren(as: List[Int], node: Int): List[Int] =
    as.zipWithIndex.filter { case (a, index) => a == node && index != node }.map(_._2)

  @tailrec
  private def parse(as: List[Int], accum: List[List[Int]], unvisited: Int): List[List[Int]] =
    if (unvisited == 0) {
      if(accum.size < as.size){
        accum.++(List.fill(as.size-accum.size)(List()))
      }else{
        accum
      }
    }
    else if (unvisited == as.size) {
      val root = findRoot(as)
      parse(as, List(List(root)), unvisited - 1)
    } else {
      val children = accum.last.map(findChildren(as, _)).flatten
      parse(as, accum.:+(children), unvisited - children.size)
    }

}
