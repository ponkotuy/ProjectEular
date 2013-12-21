
object Prob1 extends App {
  val count = 1000
  val sum = (3 until count).view.filter { i => (i % 3 == 0) || (i % 5 == 0) }.sum
  println(sum)
}
