package baml


object Solution1 {

  def solution(X: Int, Y: Int, A: Array[Int]): Int = {
    var N: Int = A.length;
    var result: Int = -1;
    var nX: Int = 0;
    var nY: Int = 0;
    var i: Int = 0;
    while (i < N) {
      if (A(i) == X)
        nX += 1;
      if (A(i) == Y)
        nY += 1;
      if (nX == nY && nX!=0)
        result = i;
      i += 1;
    }
    return result;
  }

}
