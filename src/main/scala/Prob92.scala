
// arriveのメモ化で更に高速化可能

object Prob92 extends App {
  assert(arrive(44) == 1)
  assert(arrive(85) == 89)

  val result = Iterator.range(1, 10000000)
    .map(arrive(_))
    .filter(_ == 89)
    .size
  println(result)

  def arrive(digit: Int): Int = {
    if(digit == 1 || digit == 89) digit
    else arrive(square(digit))
  }

  def square(digit: Int): Int = {
    if(digit == 0) 0
    else {
      val n = digit % 10
      n * n + square(digit / 10)
    }
  }
}
