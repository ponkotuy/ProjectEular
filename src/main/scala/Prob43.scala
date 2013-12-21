
object Prob43 {
  val divs = (Set(1) ++ Prime.set(17)).toList.sorted

  def withProperty(n: Long): Boolean = {
    val ns = n.toString.map(_ - '0')
    ns.sliding(3).zip(divs.toIterator).map { case (xs, div) =>
      val orig = xs.reduce{ (x, y) => x*10 + y }
      orig % div == 0
    }.forall(identity)
  }

  lazy val pandigital: List[Long] =
    "0123456789".permutations.filter{ _.head != '0' }.map(_.toLong).toList

  def main(args: Array[String]) {
    assert(withProperty(1406357289))
    val result = pandigital.par.filter(withProperty).toList
    println(result)
    println(result.sum)
  }
}
