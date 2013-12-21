
object Prob6 extends App {
  val num = 100
  val left = (1 to num).map{ i => i*i }.sum
  val right = (1 to num).sum
  println(right*right - left)
}
