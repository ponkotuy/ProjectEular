
object Prob20 {
  def factorial(x: Int) = (BigInt(1) to BigInt(x)).product

  def digitSum(x: BigInt) = {
    Stream.iterate((x, 0)){
      case (rest, ans) => (rest/10, ans + (rest%10).toInt)
    }.dropWhile {
      case (x, _) if x == BigInt(0) => false
      case _ => true
    }.head._2
  }

  def main(args: Array[String]) {
    println( digitSum( factorial(100) ) )
  }
}
