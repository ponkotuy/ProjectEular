
import scala.collection.BitSet

object Prob56 {
  // from Prob48
  def toBitSet(num: BigInt): BitSet = {
    val bits = Stream.from(0)
      .takeWhile(1 << _ <= num)
      .filter { i => (num & 1 << i) > 0 }
    BitSet(bits:_*)
  }

  def power(base: BigInt, exp: BigInt): BigInt = {
    val powers = Stream.iterate(base) { n => n*n }
    val bits = toBitSet(exp)
    bits.map { i => powers(i) }.product
  }

  // from Prob55
  private def intToList(n: BigInt): List[Int] = n match {
    case x if x == BigInt(0) => Nil
    case x => (x % 10).toInt :: intToList(x/10)
  }

  def powerfulDigitSum(base: BigInt, exp: BigInt): Int = {
    val value = power(base, exp)
    intToList(value).sum
  }

  def main(args: Array[String]) {
    assert(powerfulDigitSum(10, 100) == 1)
    assert(powerfulDigitSum(100, 100) == 1)
    val result = for {
      a <- 2 until 100
      b <- 2 until 100
    } yield powerfulDigitSum(a, b)
    println(result.max)
  }
}
