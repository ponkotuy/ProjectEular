
class Factorization(max: Int) {
  val primes = Prime.primes(max)

  def apply(x: Int): List[Int] = {
    val sqrt = math.sqrt(x.toDouble).toInt
    def f(rest: Int, now: Int): List[Int] = {
      if(rest == 1) return Nil
      if(rest%now == 0) now :: f(rest/now, now)
      else f(rest, now + 1)
    }
    f(x, 2)
  }
}
