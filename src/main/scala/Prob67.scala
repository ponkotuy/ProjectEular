import scalax.file.Path

/**
 * User: yosuke
 * Date: 13/11/28
 * Time: 8:30
 */
object Prob67 extends App {
  val triangle = ReverseTriangle.fromFile(Path("resources/triangle.txt", '/'))
  val maximum = triangle.rawSeq.reverse.reduce { (lower, upper) =>
    upper.zipWithIndex.map { case (x, i) =>
      math.max(lower(i), lower(i + 1)) + x
    }
  }
  println(maximum.head)
}

case class ReverseTriangle(rawSeq: IndexedSeq[Array[Int]]) {
  def top = rawSeq(0)(0)
  def apply(posy: Int) = rawSeq(posy)
  def height: Int = rawSeq.length
}

object ReverseTriangle {
  /**
   * Import File to memory
   * @param path : Import File Name
   * @return
   */
  def fromFile(path: Path): ReverseTriangle = {
    val raw = path.lines() map { line =>
      line.split(' ').map(_.toInt)
    }
    ReverseTriangle(raw.toIndexedSeq)
  }
}