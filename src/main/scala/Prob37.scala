
import scala.collection.immutable.BitSet

object Prob37 {
  val primes = Prime.bitset(10000000)
  val evens = Array('0', '4', '6', '8')
  def isTruncate(n: Int): Boolean = {
    require(primes.contains(n))
    val str = n.toString
    if(str.exists(evens.contains)) return false
    val lefts = str.tails.drop(1).filter(_ != "").map(_.toInt)
    if(!lefts.forall(primes.contains)) return false
    val rights = str.inits.drop(1).filter(_ != "").map(_.toInt)
    rights.forall(primes.contains)
  }

  def main(args: Array[String]) {
    assert(isTruncate(3797))
    val result = primes.toStream.dropWhile(_ < 10).filter(isTruncate).take(11)
    println(result.sum)
    assert(result.length == 11)
  }
}
