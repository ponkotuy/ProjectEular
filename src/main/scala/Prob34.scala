
object Prob34 {
  lazy val fact: Seq[Int] = (0 to 9).map { n => (1 to n).product }
  def check(n: Int) = {
    val sum = List.iterate(1, 7)(_*10).map { d =>
      n / d % 10
    }.reverse.dropWhile(_ == 0).map(fact).sum
    sum == n
  }

  def main(args: Array[String]) {
    assert(check(145))
    val result = (3 until fact(9)*7).par.filter(check).sum
    println(result)
  }
}
