package unified

import scala.annotation.tailrec

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Solution1 {
  def solution(Y: Int, A: String, B: String, W: String): Int = {
    // write your code in Scala 2.10
    val firstDayOfA = firstDayOfMonth(A, Y, W)
    val daysExcludedInA = daysToNextWholeWeek(firstDayOfA)
    val dayCountBetAB = inclusiveDayCount(A, B, Y)
    (dayCountBetAB - daysExcludedInA) / 7
  }

  val monthsOfYear = List("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  def dayCountOfMonth(month: String, year: Int): Int = monthsOfYear.indexOf(month) match {
    case 0 | 2 | 4 | 6 | 7 | 9 | 11=> 31
    case 1 => if (year%4==0) 29 else 28
    case 3 | 5 | 8 | 10 => 30
  }
  def firstDayOfMonth(month: String, year: Int, firstDayOfYear: String): String = {
    val daysFromStartOfYear = monthsOfYear
      .take(monthsOfYear.indexOf(month))
      .map(dayCountOfMonth(_,year))
      .sum

    val offset = daysFromStartOfYear % 7

    offsetDays(offset, firstDayOfYear)
  }

  def inclusiveDayCount(startMonth: String, endMonth: String, year: Int): Int = {
    monthsOfYear
      .slice(monthsOfYear.indexOf(startMonth), monthsOfYear.indexOf(endMonth)+1)
      .map(dayCountOfMonth(_, year))
      .sum
  }

  val daysOfWeek = List("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
  def daysToNextWholeWeek(day: String): Int = daysOfWeek.size - daysOfWeek.indexOf(day)
  def nextDayOfWeek(day: String): String = daysOfWeek.indexOf(day) match {
    case 6 => daysOfWeek.head
    case default => daysOfWeek(default+1)
  }

  @tailrec
  def offsetDays(offset: Int, day: String): String =
    if (offset==0) day else offsetDays(offset-1, nextDayOfWeek(day))
}
