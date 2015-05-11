
import scala.collection.immutable.BitSet

object Prime {
  def noPrime(max: Int): BitSet = {
    val builder = BitSet.newBuilder
    builder.sizeHint(max + 1)
    builder += 0
    builder += 1
    val sqrt = math.sqrt(max.toDouble).toInt
    (2 to sqrt).foreach { i =>
      if(!builder.result().apply(i)) Range(i*i, max+1, i).foreach(builder.+=)
    }
    builder.result()
  }

  def primes(max: Int) = {
    val noPrime = this.noPrime(max)
    (2 to max).filter(!noPrime(_))
  }

  def bitset(max: Int): BitSet = {
    val fill = fillBitSet(max + 1)
    fill ^ noPrime(max)
  }

  def set(max: Int): Set[Int] = {
    (0 to max).toSet -- noPrime(max)
  }

  private def fillBitSet(max: Int): BitSet = {
    val builder = BitSet.newBuilder
    builder.sizeHint(max)
    (0 until max).foreach(builder.+=)
    builder.result()
  }

  def isPrime(primes: Traversable[Int])(x: Long): Boolean = {
    val max = math.sqrt(x.toDouble)
    primes.takeWhile(_ <= max).forall(x % _ != 0)
  }
}
