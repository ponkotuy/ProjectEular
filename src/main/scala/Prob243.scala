import scala.collection.mutable

object Prob243 extends App {
  val primeMax = Int.MaxValue / 32
  val multiPrimes: Stream[Int] = 1 #:: Prime.primes(23).toStream.zip(multiPrimes).map { case (x, y) => x * y }
  val multiPrimesWithPrime = multiPrimes.tail.zip(Prime.primes(23))
  assert(resilient(12, Set(2, 3)) == 4)
  println(15499 / 94744.0)
  minResilient(15499 / 94744.0)

  def minResilient(border: Double): Int = {
    def f(n: Int, i: Int, elems :Set[Int], primes: List[Int]): Int = {
      val v = n * i
      val result = resilient(v, elems) / (v - 1).toDouble
      println(v, result)
      if(result < border) v
      else {
        primes match {
          case p :: ps => f(n * p, 1, elems + p, ps)
          case Nil => f(n, i + 1, elems, primes)
        }
      }
    }
    f(2, 1, Set(2), Prime.primes(23).tail.toList)
  }

  def resilient(n: Int, elems: Set[Int]): Int = {
    val bitset = mutable.BitSet()
    elems.foreach { elem =>
      Range(elem, n, elem).foreach(bitset += _)
    }
    n - bitset.size - 1
  }

  def commonDivisor(x: Int, y: Int): Int = {
    def f(max: Int, min: Int): Int = {
      if (min == 1) return 1
      val reminder = max % min
      if (reminder == 0) min else f(min, reminder)
    }
    val max = math.max(x, y)
    val min = math.min(x, y)
    f(max, min)
  }
}
