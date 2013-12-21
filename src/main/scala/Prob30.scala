
object Prob30 extends App {
  def f(xs: Array[Int]): Boolean = {
    val powers = xs.map { x =>
      List.fill(5)(x).product
    }.sum
    powers == fromArray(xs)
  }

  def toArray(x: Int): Array[Int] =
    x.toString.map(_ - '0').toArray

  def fromArray(xs: Array[Int]) = xs.reduce(_ * 10 + _)

  println((2 to 354294).par.map(toArray).filter(f).map(fromArray).sum)
}
