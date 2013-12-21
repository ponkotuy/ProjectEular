
object Prob17 {
  val Ones = Vector(0, 3, 3, 5, 4, 4, 3, 5, 5, 4)
  val Teens = Vector(3, 6, 6, 8, 8, 7, 7, 9, 8, 8)
  val Tens = Vector(0, 3, 6, 6, 5, 5, 5, 7, 6, 6)
  val And = 3
  val Hundred = 7
  val Thousand = 8

  def toLetterCount(number: Int): Int = {
    if(number == 1000) return Thousand + Ones(1)
    val hand: Int = number / 100 match {
      case 0 => 0
      case n => Ones(n) + Hundred
    }
    val ten: Int = number % 100 / 10 match {
      case 0 => 0
      case 1 =>
        val one = number % 10
        Teens(one) - Ones(one)
      case n => Tens(n)
    }
    val one: Int = Ones(number % 10)
    val and: Int = if(hand > 0 && (ten > 0 || one > 0)) And else 0
    hand + ten + one + and
  }

  def main(args: Array[String]) {
    assert(toLetterCount(1) == 3)
    assert(toLetterCount(5) == 4)
    assert(toLetterCount(10) == 3)
    assert(toLetterCount(11) == 6)
    assert(toLetterCount(18) == 8)
    assert(toLetterCount(19) == 8)
    assert(toLetterCount(20) == 6)
    assert(toLetterCount(21) == 9)
    assert(toLetterCount(100) == 10)
    assert(toLetterCount(101) == 16)
    assert(toLetterCount(115) == 20)
    assert(toLetterCount(342) == 23)
    assert(toLetterCount(1000) == 11)
    println((1 to 1000).map(toLetterCount).sum)
  }
}
