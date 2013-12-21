
object Prob21 {
  def divisors(x: Int): List[Int] = {
    require(x > 0)
    val upper = math.sqrt(x.toDouble).toInt
    (1 to upper).filter {
      i => x % i == 0
    }.flatMap{
      i => List(i, x / i)
    }.toList
  }

  def d(x: Int): Int = {
    divisors(x).sum - x
  }

  def main(args: Array[String]) {
    assert(d(220) == 284)
    assert(d(284) == 220)
    val sum = (2 until 10000).filter { i =>
      val result = d(i)
      d(result) == i && result != i
    }.sum
    println(sum)
  }
}
