package baml

import scala.collection.JavaConversions._

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution2 {
  def solution(S: String): Int = {
    S.split(Array('.','?','!'))
      .map(sentence=> sentence.trim.split(" ").filterNot(_.isEmpty).size)
      .max
  }
}
