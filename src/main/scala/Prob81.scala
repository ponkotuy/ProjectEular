import scala.io.Source

object Prob81 extends App {
  val matrix = Source.fromFile("resources/p081_matrix.txt").getLines().map { line =>
    line.split(',').map(_.toInt)
  }.toArray
  val xSize = matrix.length
  val ySize = matrix.head.length

  for {
    j <- matrix.indices
    i <- matrix.head.indices
  } {
    if(i == 0 && 0 < j) matrix(j)(i) += matrix(j - 1)(i)
    else if(0 < i && j == 0) matrix(j)(i) += matrix(j)(i - 1)
    else if(0 < i && 0 < j) matrix(j)(i) += math.min(matrix(j - 1)(i), matrix(j)(i - 1))
  }
  println(matrix(ySize - 1)(xSize - 1))
}

