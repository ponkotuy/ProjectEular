
object Prob51 {
  val primes = Prime.set(1000000)
  val listPrimes = Prime.primes(1000000)

  /**
    * @param: base number
    * @param: change point example 43210 from the right side
    */
  def primeCount(n: Int)(changes: List[Int]): Int = {
    val ary = intToList(n).toArray
    val start = if(changes.contains(ary.length - 1)) 1 else 0
    (start to 9).filter { i =>
      changes.foreach(ary(_) = i)
      val result = listToInt(ary.toList)
      primes(result)
    }.size
  }

  private def intToList(n: Int): List[Int] = n match {
    case 0 => Nil
    case x => (x % 10) :: intToList(x/10)
  }

  private def listToInt(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: xs => x + 10*listToInt(xs)
  }

  def exec(n: Int): Int = {
    val list = intToList(n)
    val dist = list.distinct
    dist.flatMap { x =>
      val idx = list.zipWithIndex
        .filter { case (y, _) => y == x }
        .map(_._2)
      (1 to idx.length).flatMap { cnt =>
        val is = idx.combinations(cnt)
        is.map(primeCount(n))
      }
    }.max
  }

  def main(args: Array[String]) {
    assert(primeCount(13)(List(1)) == 6)
    assert(exec(13) == 6)
    assert(primeCount(56003)(List(1, 2)) == 7)
    assert(exec(56993) == 7)
    val result = listPrimes.dropWhile(_ < 56003)
      .map(it => it -> exec(it))
      .filter(_._2 == 8)
    println(result.head)
  }
}
