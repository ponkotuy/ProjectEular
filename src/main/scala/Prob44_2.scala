
object Prob44_2 {
  def pentagonal(n: Int) = n*(3*n - 1)/2
  val pentagonals = Stream.from(0).map(pentagonal).takeWhile(_ < (Int.MaxValue >> 2)).toList

  def isPenta(n: Int) = {
    val dbl = (1.0 + math.sqrt(1 + 24*n))/6.0
    val int = dbl.toInt
    pentagonal(int) == n || pentagonal(int + 1) == n
  }

  def check(j: Int, k: Int): Boolean = {
    val pj = pentagonals(j)
    val pk = pentagonals(k)
    isPenta(pj + pk) && isPenta((pj - pk).abs)
  }

  def main(args: Array[String]) {
    val result = for {
      i <- 1 to 10000
      j <- (i + 1) to 10001
      if check(i, j)
    } yield (i, j)
    println(result.head)
  }
}
