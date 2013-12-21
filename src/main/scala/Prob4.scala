
object Prob4 {
  def main(args: Array[String]) {
    val result = (
      for {
        i <- Range(999, 100, -1)
        j <- Range(i, 100, -1)
        if isPalindrome(i*j)
      } yield (i*j)
    ).max
    println(result)
  }

  def isPalindrome(x: Int): Boolean = {
    val str = x.toString
    str.zip(str.reverse).forall{ it => it._1 == it._2 }
  }
}
