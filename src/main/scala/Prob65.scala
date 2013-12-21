
/**
 * User: yosuke
 * Date: 13/10/20
 * Time: 3:24
 */
object Prob65 {
  def main(args: Array[String]) {
    val sqrt2 = ContinuedFraction(1, Stream.continually(2))
    assert(sqrt2.calc(1) == Fraction(3, 2))
    assert(sqrt2.calc(2) == Fraction(7, 5))
    assert(sqrt2.calc(3) == Fraction(17, 12))
    assert(sqrt2.calc(4) == Fraction(41, 29))
    val eTerms = Stream.from(1).flatMap(i => Stream(1, i*2, 1))
    assert(eTerms.take(9) == Stream(1, 2, 1, 1, 4, 1, 1, 6, 1))
    val exponential = ContinuedFraction(2, eTerms)
    assert(exponential.calc(1) == Fraction(3))
    assert(exponential.calc(2) == Fraction(8, 3))
    assert(exponential.calc(3) == Fraction(11, 4))
    assert(exponential.calc(4) == Fraction(19, 7))
    assert(exponential.calc(5) == Fraction(87, 32))
    assert(exponential.calc(6) == Fraction(106, 39))
    assert(exponential.calc(7) == Fraction(193, 71))
    assert(exponential.calc(8) == Fraction(1264, 465))
    assert(exponential.calc(9) == Fraction(1457, 536))
    val result = exponential.calc(99)
    println(result)
    println(result.num.toString().map(_ - '0').sum)
  }

  case class ContinuedFraction(start: Int, terms: Stream[Int]) {
    def calc(th: Int): Fraction = {
      val right = terms.take(th)
        .map(BigInt.apply)
        .map(Fraction.apply)
        .reduceRight[Fraction] { case (next, rest) =>
        next + rest.inverse
      }
      right.inverse + start
    }
  }

  class Fraction(val num: BigInt, val denom: BigInt) {
    def inverse: Fraction = Fraction(denom, num)
    def unary_+ = this
    def unary_- = Fraction(-num, denom)

    def +(other: Fraction): Fraction =
      Fraction(num*other.denom + denom*other.num, denom*other.denom)
    def +(other: Int): Fraction = this + Fraction(other)
    def -(other: Fraction): Fraction =
      this + (-other)
    def *(other: Fraction): Fraction =
      Fraction(num*other.num, denom*other.denom)
    def /(other: Fraction): Fraction =
      this*other.inverse

    override def equals(arg0: Any): Boolean = arg0 match {
      case Fraction(num_, denom_) =>
        num_ == num && denom_ == denom
      case _ => false
    }

    override def toString = s"$num/$denom"
  }

  object Fraction {
    def apply(num: BigInt): Fraction = new Fraction(num, 1)
    def apply(num: BigInt, denom: BigInt): Fraction = {
      require(denom != BigInt(0))
      if(num == BigInt(0)) return apply(0)
      val minus = if((num < 0) ^ (denom < 0)) -1 else 1
      val elem = gcd(num.abs, denom.abs)
      new Fraction(num.abs/elem*minus, denom.abs/elem)
    }

    def unapply(frac: Fraction) = Some((frac.num, frac.denom))

    def ZERO: Fraction = apply(0)

    private def gcd(x: BigInt, y: BigInt): BigInt = {
      require(x > 0)
      require(y > 0)
      val max = x.max(y)
      val min = x.min(y)
      val reminder = max % min
      if(reminder == BigInt(0)) min
      else gcd(min, reminder)
    }
  }
}
