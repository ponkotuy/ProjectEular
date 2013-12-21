
object Prob53 {
  val factorial: Stream[BigInt] =
    BigInt(1) #:: factorial.zipWithIndex.map { case (x, i) => BigInt(i + 1)*x }

  def combinatoric(n: Int, r: Int): BigInt =
    factorial(n) / (factorial(r)*factorial(n - r))

  def main(args: Array[String]) {
    assert(combinatoric(5, 3) == 10)
    assert(combinatoric(23, 10) == 1144066)
    val cs = for {
      n <- 3 to 100
      r <- 2 to n
    } yield combinatoric(n, r)
    println(cs.filter(_ > 1000000).length)
  }
}
