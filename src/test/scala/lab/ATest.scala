package lab

import org.scalatest._

class ATest extends FlatSpec with Matchers {

  "A" should "reverse a valid input" in {
    A.reverse("test") should be ("tset")
  }

  it should "fail if input is null" in {
    intercept[Exception](A.reverse(null))
  }

}