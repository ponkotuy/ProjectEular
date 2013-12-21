import scala.collection.mutable

/**
 * User: yosuke
 * Date: 13/10/15
 * Time: 2:32
 */
object Prob60 {

  def main(args: Array[String]) {
    val pp = new PrimePairs(10000)
    assert(f(pp, pp.containPrimes.toSet, 3).contains(List(3, 7, 109, 673)))
    val result = f(pp, pp.containPrimes.toSet, 4)
    println(result)
  }

  def f(pp: PrimePairs, xs: Set[Int], step: Int): Set[List[Int]] = {
    if(step == 0) return xs.map(List(_))
    xs flatMap { x =>
      val ys = pp.fromLeft(x)
      f(pp, xs & ys, step - 1) map { list =>
        x :: list
      }
    }
  }
}

class PrimePairs(max: Int) {
  val primeMax = math.min(max*max, 10000000)
  val primeSet = Prime.bitset(primeMax)
  val primeSeq = Prime.primes(primeMax).toStream

  val containPrimes = primeSeq.takeWhile(_ <= max)
  val pairs: mutable.MultiMap[Int, Int] = createPairs()

  private def createPairs(): mutable.MultiMap[Int, Int] = {
    val map = new mutable.HashMap[Int, mutable.Set[Int]] with mutable.MultiMap[Int, Int]
    for {
      x <- containPrimes
      y <- containPrimes
      if y > x
      if isPrimePair(x, y)
    } map.addBinding(x, y)
    map
  }

  def isPrimePair(x: Int, y: Int): Boolean = {
    require(primeSet.contains(x))
    require(primeSet.contains(y))
    isPrime(join(x, y)) && isPrime(join(y, x))
  }

  private[this] def join(x: Int, y: Int): Long =
    (x.toString + y.toString).toLong

  def fromLeft(x: Int): Set[Int] =
    pairs.getOrElse(x, Set()).toSet

  def isPrime(x: Long): Boolean = {
    if(x <= primeMax) primeSet.contains(x.toInt)
    else Prime.isPrime(primeSeq)(x)
  }
}