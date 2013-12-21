
object Prob12 extends App {
  def divisors(x: Long): List[Long] = {
    require(x > 0L)
    val upper = math.sqrt(x.toDouble).toInt
    (1 to upper).filter {
      i => x % i == 0L
    }.flatMap{
      i => List(i, x / i)
    }.toList
  }
  val triangle: Stream[Int] = 1 #:: Stream.from(2).zip(triangle).map { it =>
    require(it._2 > 0)
    it._1 + it._2
  }
  println(triangle.filter{ it => divisors(it).size > 500 }.head)
}
