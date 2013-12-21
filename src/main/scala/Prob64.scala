import scala.collection.mutable

/**
 * User: yosuke
 * Date: 13/10/17
 * Time: 1:51
 */
object Prob64 {
  import ContinuedFraction.expansionFrac

  def main(args: Array[String]) {
    assert(expansionFrac(23).take(8).map(_._1) == Stream(4, 1, 3, 1, 8, 1, 3, 1))
    assert(oddPeriodCount(13) == 4)
    println(oddPeriodCount(10000))
  }

  def oddPeriodCount(max: Int): Int = {
    val squares = Stream.from(1).map(i => i * i).takeWhile(_ <= max).toSet
    (2 to max).filter(!squares.contains(_)).map(countPeriod).count(_ % 2 != 0)
  }

  def countPeriod(n: Int): Int = {
    val stream = expansionFrac(n).zipWithIndex
    val set = mutable.Set[(Int, Fraction)]()
    val result = (stream find { case (dat, i) =>
      if(set.contains(dat)) true
      else {
        set += dat
        false
      }
    }).get
    val after = result._2
    val before = stream.find(_._1 == result._1).get._2
    after - before
  }

  /**
   * Continued Fractions(連分数) Tool
   * User: yosuke
   * Date: 13/10/17
   * Time: 4:44
   */
  object ContinuedFraction {
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