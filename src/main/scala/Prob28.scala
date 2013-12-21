
object Prob28 extends App {
  def f(n: Long) = (16*n + 36)*n + 24
  val results = 1 #:: Stream.from(0).map(_.toLong).map(f)

  val x = 1001 / 2
  println(results.take(x + 1).sum)
}
