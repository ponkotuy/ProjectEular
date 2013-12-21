
import scala.collection.{ BitSet, breakOut }
import scala.collection.mutable

object Prob23 {
  def divisors(x: Int): Set[Int] = {
    require(x > 0)
    val upper = math.sqrt(x.toDouble).toInt
    (1 to upper).filter {
      i => x % i == 0
    }.flatMap{
      i => List(i, x / i)
    }.toSet
  }

  def isAbundant(x: Int): Boolean =
    (divisors(x).sum - x) > x

  val abundant = Stream.from(1).filter(isAbundant)

  def abundantSumSets(max: Int): mutable.BitSet = {
    val bitset = mutable.BitSet()
    for {
      n <- abundant.takeWhile(_ < max)
      m <- abundant.takeWhile(_ < max)
    } {
      bitset += n + m
    }
    return bitset
  }

  def main(args: Array[String]) {
    val max = 28123
    assert(abundant.head == 12)
    val data = abundantSumSets(max)
    assert(data(24))
    println( (1 to max).filter(!data(_)) )
    println( (1 to max).filter(!data(_)).sum )
  }
}
