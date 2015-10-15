
object Prob71 extends App {
  val result = (2 to 1000000).par.flatMap { d =>
    val nMax = (3 * d - 1) / 7
    val n = Stream.range(nMax, 1, -1).filter { n => gcd(d, n) == 1 }.headOption
    n.map(_ -> d)
  }.maxBy { case (n, d) => n.toDouble / d }
  println(result)

  def gcd(max: Int, min: Int): Int = {
    if(min == 0) max else gcd(min, max % min)
  }
}
