
object Prob35 {
  val primes = Prime.primes(9999999)
  val evens = Array('0', '2', '4', '6', '8')

  def circle(xs: String) =
    Seq.iterate(xs, xs.length) { it =>
      it.tail ++ it.head.toString
    }

  def isCircular(n: Int): Boolean = {
    require(primes.contains(n))
    if(n < 10) return true
    val str = n.toString
    val even = str.exists(evens.contains)
    if(even) return false
    circle(str).map(_.toInt).forall(primes.contains)
  }

  def main(args: Array[String]) {
    assert(isCircular(79))
    assert(!isCircular(23))
    assert(primes.takeWhile(_ < 100).filter(isCircular).length == 13)
    println(primes.takeWhile(_ < 1000000).par.filter(isCircular).length)
  }
}
