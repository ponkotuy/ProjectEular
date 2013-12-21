import scala.collection.mutable

/**
 * User: yosuke
 * Date: 13/10/21
 * Time: 3:53
 */
object Prob66_2 {
  def main(args: Array[String]) {
    assert(ContinuedFraction.fromSquare(23) == ContinuedFraction(4, List(1, 3, 1, 8)))
    println(minAnsPellEq(14))
    assert(minAnsPellEq(7) == (8, 3))
    assert(minAnsPellEq(14) == (15, 4))
    val squares = Stream.from(1).map(i => i*i).takeWhile(_ <= 1000).toSet
    println((7 to 1000).filterNot(squares.contains).map(i => i -> minAnsPellEq(i)._1).maxBy(_._2))
  }

//  def ansPellEq(n: Int): Stream[(BigInt, BigInt)] = {
//    val bigN = BigDecimal(n)
//    val min = minAnsPellEq(n)
//    val result: Stream[(BigInt, BigInt)] =
//      Stream.cons(min, result.zipWithIndex map { case ((x, y), i) =>
//        (BigDecimal(x) + bigN*BigDecimal(y)).pow(i + 2)
//      } )
//  }

  /**
   * Get Minimum x*x - n*y*y = 1 (Pell's equation) Answer
   * @return (x, y)
   */
  def minAnsPellEq(n: Int): (BigInt, BigInt) = {
    val continued = ContinuedFraction.fromSquare(n)
    val qs = continued.start :: continued.periodic.init
    lazy val xs: Stream[BigInt] = BigInt(1) #:: BigInt(qs(0)) #:: xs.zip(xs.tail).zip(qs.tail).map { case ((fst, snd), q) =>
      fst + q*snd
    }
    lazy val ys: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: ys.zip(ys.tail).zip(qs.tail).map { case ((fst, snd), q) =>
      fst + q*snd
    }
    (xs.last, ys.last)
  }

  case class ContinuedFraction(start: Int, periodic: List[Int])
  /**
   * Continued Fractions(連分数) Tool
   * User: yosuke
   * Date: 13/10/17
   * Time: 4:44
   */
  object ContinuedFraction {
    def fromSquare(n: Int): ContinuedFraction = {
      val expFrac = expansionFrac(n)
      val stream = expFrac.zipWithIndex
      val set = mutable.Set[(Int, Fraction)]()
      val result = (stream find { case (dat, i) =>
        if(set.contains(dat)) true
        else {
          set += dat
          false
        }
      }).get
      val until = result._2
      val from = stream.find(_._1 == result._1).get._2
      val periodic = expFrac.map(_._1).slice(from, until).toList
      ContinuedFraction(stream.head._1._1, periodic)
    }

    def expansion(n: Int): Stream[Int] = {
      def f(frac: Fraction): Stream[Int] = {
        val integ = frac.integral
        val next = Fraction(n, frac.wholeNum - integ*frac.denom, frac.denom)
        integ #:: f(next.inverse)
      }
      f(Fraction(n, 0, 1))
    }

    def expansionFrac(n: Int): Stream[(Int, Fraction)] = {
      def f(frac: Fraction): Stream[(Int, Fraction)] = {
        val integ = frac.integral
        val next = Fraction(n, frac.wholeNum - integ*frac.denom, frac.denom)
        (integ, next) #:: f(next.inverse)
      }
      f(Fraction(n, 0, 1))
    }
  }

  case class Fraction(sqrtNum: Int, wholeNum: Int, denom: Int) {
    def integral: Int =
      math.floor((math.sqrt(sqrtNum) + wholeNum)/denom).toInt

    def inverse: Fraction = {
      val newDenom = sqrtNum - wholeNum*wholeNum
      Fraction(sqrtNum, -wholeNum, newDenom/denom)
    }
  }

}
