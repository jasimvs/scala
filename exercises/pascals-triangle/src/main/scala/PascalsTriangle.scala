package jasim

object PascalsTriangle {

  def rows(number: Int): List[List[Int]] = {
    (1 to number).foldLeft(List.empty[List[Int]])((acc, n) => acc :+ buildNthRow(acc, n))
  }

  private def buildNthRow(rows: List[List[Int]], n: Int) = {
    n match {
      case 1 => List(1)
      case 2 => List(1, 1)
      case _ =>
        val previousRow = rows(n - 2)
        1 :: previousRow.zip(previousRow.tail)
          .foldRight(List(1))((pair, acc) => pair._1 + pair._2 :: acc)
    }
  }
}