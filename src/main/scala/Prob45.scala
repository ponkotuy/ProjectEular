
object Prob45 {
  val triangle =
    Stream.from(0).map(n => n*(n + 1L)/2).takeWhile(_ < (Int.MaxValue)).toSet
  val pentagonal =
    Stream.from(0).map(n => n*(3L*n - 1)/2).takeWhile(_ < (Int.MaxValue)).toSet
  def hexagonal(n: Int) = n*(2L*n - 1)

  def main(args: Array[String]) {
    assert(triangle(hexagonal(143)))
    assert(pentagonal(hexagonal(143)))
    val result = Stream.from(144).filter { i =>
      val hexa = hexagonal(i)
      require(hexa > 0)
      if(i % 1000 == 0) println(hexa)
      triangle(hexa) && pentagonal(hexa)
    }.head
    println(result, hexagonal(result))
  }
}
