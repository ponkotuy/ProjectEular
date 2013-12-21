
import scala.collection.BitSet

object Prob48 {
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

  def main(args: Array[String]) {
    assert((1 to 10).map { i => power(i, i) }.sum == BigInt(10405071317L))
    val result = (1 to 1000).map { i => power(i, i) }.sum
    println(result.toString.takeRight(10))
  }
}
