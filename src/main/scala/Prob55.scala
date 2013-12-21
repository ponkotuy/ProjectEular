
object Prob55 {
  private def intToList(n: BigInt): List[Int] = n match {
    case x if x == BigInt(0) => Nil
    case x => (x % 10).toInt :: intToList(x/10)
  }

  private def listToInt(xs: List[Int]): BigInt = xs match {
    case Nil => BigInt(0)
    case x :: xs => BigInt(x) + BigInt(10)*listToInt(xs)
  }

  private def iReverse(n: BigInt): BigInt = {
    val list = intToList(n).reverse
    listToInt(list)
  }

  private def isPalindrome(n: BigInt): Boolean = {
    listToInt(intToList(n).reverse) == n
  }

  def isLychrelNumber(num: Int, max: Int = 50): Boolean =
    Stream.iterate(BigInt(num), max) { n => n + iReverse(n) }.tail
      .filter(isPalindrome).isEmpty

  def main(args: Array[String]) {
    assert(!isLychrelNumber(47))
    assert(!isLychrelNumber(349))
    assert(isLychrelNumber(196))
    val result = Stream.range(196, 10000).filter(isLychrelNumber(_))
    println(result.length)
  }
}
