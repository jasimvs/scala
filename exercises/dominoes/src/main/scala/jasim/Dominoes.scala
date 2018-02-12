package jasim


object Dominoes {
  def chain(input: List[(Int, Int)]): Option[List[(Int, Int)]] = {

    if (input.isEmpty)
      Option(input)
    else {
      input.permutations.find(list => {
        val tuples = list.zip(list.tail)
        if (tuples.isEmpty)
          list.head._1 == list.last._2
        else
          tuples.head._1._1 == tuples.last._2._2 &&
            tuples.find(pair => pair._1._2 != pair._2._1).isEmpty
      })
    }
  }
}