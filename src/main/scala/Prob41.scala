
object Prob41 {
  private def toList(n: Int): List[Int] = {
    def f(n: Int): List[Int] = n match {
      case 0 => Nil
      case _ => (n % 10) :: f(n / 10)
    }
    f(n).reverse
  }

  def isPandigital(n: Int): Boolean = {
    val xs = toList(n)
    xs.toSet == (1 to xs.length).toSet
  }

  def main(args: Array[String]) {
    assert(isPandigital(2143))
    val primes = Prime.primes(987654321)
    println(primes.par.filter(isPandigital))
  }
}
