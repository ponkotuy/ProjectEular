
object Prob27 {
  def quadratic(a: Int, b: Int): Stream[Int] =
    Stream.from(0).map { n => n*n + a*n + b }

  def main(args: Array[String]) {
    val primes = Prime.primes(100000)
    val lengths = for {
      a <- (-999 to 999)
      b <- primes.takeWhile(_ < 1000).par // n = 0が素数になる条件
      if (a + b + 1) >= 2                 // n = 1が2より大きくなる条件
    } yield {
      val result = quadratic(a, b).takeWhile(primes.contains).length
      (a, b, result)
    }
    println(lengths.maxBy(_._3))
  }
}
