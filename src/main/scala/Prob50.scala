
object Prob50 {
  val setPrime = Prime.set(1000000)
  val primes = Prime.primes(1000000).toStream

  // return (sum, count)
  def consPrimeSum(max: Int)(start: Int): (Int, Int) = {
    val result = primes.dropWhile(_ < start).takeWhile(_ < max)
      .foldLeft[List[Long]](Nil) { case (before, x) => (before.headOption.getOrElse(0L) + x) :: before }
      .dropWhile(_ >= max)
      .map(_.toInt)
      .dropWhile(!setPrime.contains(_))
    (result.head, result.length)
  }

  // return (sum, count)
  def exec(max: Int): (Int, Int) = {
    primes.take(1000).par.takeWhile(_ < max)
      .map(consPrimeSum(max))
      .maxBy { case (_, count) => count }
  }

  def main(args: Array[String]) {
    assert(exec(100) == (41, 6))
    assert(exec(1000) == (953, 21))
    println(exec(1000000))
  }
}
