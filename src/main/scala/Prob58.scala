
import scala.math._

object Prob58 {

  val spiral = {
    val increments: Stream[Long] = Stream.from(1) flatMap { i: Int =>
      Seq.fill(4)(i*2L)
    }
    increments.scan(1L)(_ + _)
  }

  private val PRIME_MAX = 500000
  private val primeSets = Prime.set(PRIME_MAX)
  private val primes = Prime.primes(PRIME_MAX)
  def isPrime(x: Long): Boolean = {
    if(x < PRIME_MAX) primeSets.contains(x.toInt)
    else {
      val sqrtx = sqrt(x.toDouble).toInt
      if(sqrtx > PRIME_MAX) throw new RuntimeException("PRIME_MAX too low")
      primes.takeWhile(_ <= sqrtx).forall(x % _ != 0)
    }
  }

  def main(args: Array[String]) {
    assert(spiral.take(13) ==
      Stream(1L, 3L, 5L, 7L, 9L, 13L, 17L, 21L, 25L, 31L, 37L, 43L, 49L))
    val rate13 = spiral.take(13).filter(isPrime).length / 13.0
    assert(eq(rate13, 0.62))
    val primeCount = spiral.scanLeft(0) { case (cnt, x) =>
      cnt + (if(isPrime(x)) 1 else 0)
    }
    val primeRate = primeCount.zipWithIndex map { case (cnt, i) => cnt.toDouble/(i + 1) }
    val ratePerSquare = primeRate.drop(1).grouped(4).map(_.last)
    println(ratePerSquare.indexWhere(_ < 0.1)*2L + 3L)
  }

  def eq(d1: Double, d2: Double, dif: Double = 0.005): Boolean =
    abs(d1 - d2) < dif
}
