package unified

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution2 {
  def solution(S: String): Int = {
    // write your code in Scala 2.10
    val calls: List[Call] = S.split("\n",-1).map(s => Call.fromString(s.trim)).toList
    val numberWithPromotion = findNumberWPromotion(calls)
    val filteredCalls = calls.filterNot(call => call.number== numberWithPromotion)
    filteredCalls.map(price).sum
  }


  case class Call(number: Long, duration: Int)
  object Call{
    def fromString(raw:String) = {
      val parts = raw.split(",")
      val durationParts = parts(0).split(":",-1)
      val duration = durationParts(2).toInt + durationParts(1).toInt*60 + durationParts(0).toInt*60*60
      val number = parts(1).replace("-","").toLong
      apply(number,duration)
    }
  }

  def price(call: Call): Int = call.duration match {
    case x if x < 300 => x*3
    case default => if ((default % 60)==0) 150*default/60 else 150*(default/60 + 1)
  }

  def findNumberWPromotion(calls: List[Call]): Long = {
    val sortedNumberGroups = calls
      .groupBy(call => call.number)
      .mapValues(calls => calls.map(_.duration).sum)
      .toList.sortWith(_._2 > _._2)

    val winnerDuration = sortedNumberGroups.head._2

    sortedNumberGroups
      .takeWhile(group => group._2 == winnerDuration)
      .map(_._1)
      .min
  }

}