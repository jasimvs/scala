package jasim


object Etl {
  def transform(scoringTable: Map[Int, Seq[String]]): Map[String, Int] = {
    scoringTable.flatMap(x => x._2.map(y => y.toLowerCase -> x._1))
  }
}