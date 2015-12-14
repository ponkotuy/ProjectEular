
import scala.collection.mutable

object Prob74 extends App {
  val chains = new Chains()
  assert(chains(145) == 1)
  assert(chains(540) == 2)
  assert(chains(69) == 5)
  println((1 to 1000000).map(chains(_)).count(59 <= _))
}

class Factorial {
  val cache = mutable.Map[Int, Int]()

  def apply(n: Int): Int = {
    if(n <= 1) 1
    else cache.getOrElseUpdate(n, n * apply(n - 1))
  }
}

class Chains {
  import Chains._

  val cache = mutable.Map[Int, Int]()
  val fact = new Factorial()

  def apply(n: Int, xs: Set[Int] = Set()): Int = {
    def f: Int = {
      val ys = xs + n
      val sum = splitNumber(n).map(fact.apply).sum
      if(ys.contains(sum)) 1 else 1 + apply(sum, ys)
    }
    cache.getOrElseUpdate(n, f)
  }
}

object Chains {
  def splitNumber(n: Int): List[Int] = {
    if(n == 0) Nil
    else {
      val remain = n % 10
      val rest = n / 10
      remain :: splitNumber(rest)
    }
  }
}
