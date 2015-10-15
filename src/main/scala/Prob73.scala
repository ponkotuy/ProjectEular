
object Prob73 extends App {
  assert(count(8) == 3)
  println(count(12000))

  def count(dMax: Int): Int = {
    (2 to dMax).par.map { d =>
      val nMin = d / 3 + 1
      val nMax = (d - 1) / 2
      (nMin to nMax).filter { n => gcd(d, n) == 1 }.size
    }.sum
  }

  def gcd(max: Int, min: Int): Int = {
    if(min == 0) max else gcd(min, max % min)
  }
}
