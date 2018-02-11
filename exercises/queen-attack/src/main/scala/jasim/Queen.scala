package jasim

case class Queen(x: Int, y: Int)

object Queen {
  def create(x: Int, y: Int) =
    if (x >= 0 && x < 8 && y >= 0 && y < 8)
      Option(Queen(x, y))
    else
      None
}

object QueenAttack {

  def canAttack(q1: Queen, q2: Queen): Boolean =
    (1 to 8).exists(n => calculateNextPositions(q1, n).contains(q2))

  private def calculateNextPositions(queen: Queen, numberOfSteps: Int): List[Queen] = {
    List(
      Queen.create(queen.x + numberOfSteps, queen.y),
      Queen.create(queen.x, queen.y + numberOfSteps),
      Queen.create(queen.x - numberOfSteps, queen.y),
      Queen.create(queen.x, queen.y - numberOfSteps),
      Queen.create(queen.x + numberOfSteps, queen.y + numberOfSteps),
      Queen.create(queen.x + numberOfSteps, queen.y - numberOfSteps),
      Queen.create(queen.x - numberOfSteps, queen.y + numberOfSteps),
      Queen.create(queen.x - numberOfSteps, queen.y - numberOfSteps)
    ).flatten
  }
}