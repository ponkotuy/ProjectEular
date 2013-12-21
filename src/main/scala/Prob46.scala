
import scala.math

object Prob46 {
  val twiceSquare = Stream.from(1).map { i => 2*i*i }
  val primes = Prime.set(10000000)

  def isGoldbach(n: Int): Boolean =
    twiceSquare.takeWhile(_ < n).exists{ i => primes.contains(n - i) }

  def main(args: Array[String]) {
    assert(Array(9, 15, 21, 25, 27, 33).forall(isGoldbach))
    val result = Stream.range(9, primes.max, 2)
      .filterNot(primes.contains)
      .filterNot(isGoldbach).head
    println(result)
  }
}
