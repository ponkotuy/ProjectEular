
object Prob36 {
  def isPalindrome(n: Int): Boolean = {
    val str10 = n.toString
    if(str10.reverse != str10) return false
    val str1 = n.toBinaryString
    str1.reverse == str1
  }

  def main(args: Array[String]) {
    assert(isPalindrome(585))
    println((1 until 1000000).filter(isPalindrome).sum)
  }
}
