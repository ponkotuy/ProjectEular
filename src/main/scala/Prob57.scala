
object Prob57 {
  case class Rational(n: BigInt, d: BigInt) {
    private def gcd(x: BigInt, y: BigInt): BigInt = {
      if (x == 0) y
      else if (x < 0) gcd(-x, y)
      else if (y < 0) -gcd(x, -y)
      else gcd(y % x, x)
    }
    private val g = gcd(n, d)
    val numer: BigInt = n/g
    val denom: BigInt = d/g
    def +(that: Rational) =
      new Rational(numer * that.denom + that.numer * denom,
        denom * that.denom)
    def -(that: Rational) =
      new Rational(numer * that.denom - that.numer * denom,
        denom * that.denom)
    def *(that: Rational) =
      new Rational(numer * that.numer, denom * that.denom)
    def /(that: Rational) =
      new Rational(numer * that.denom, denom * that.numer)
  }

  def sq2(ac: Int): List[Rational] = {
    List.iterate(Rational(2, 1), ac + 1){ before =>
      Rational(before.denom + 2*before.numer, before.numer)
    }.map(_ - Rational(1, 1))
  }

  def main(args: Array[String]) {
    val result = sq2(1000)
    assert(result(1) == Rational(3, 2))
    assert(result(2) == Rational(7, 5))
    assert(result(3) == Rational(17, 12))
    assert(result(4) == Rational(41, 29))
    assert(result(5) == Rational(99, 70))
    assert(result(6) == Rational(239, 169))
    assert(result(7) == Rational(577, 408))
    assert(result(8) == Rational(1393, 985))
    val count = result.count { it =>
      it.numer.toString.length > it.denom.toString.length
    }
    println(count)
  }
}
