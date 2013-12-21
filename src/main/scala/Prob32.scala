
object Prob32 extends App {
  def fil(x: Int, y: Int): Boolean = {
    if(x*y > 9876) return false
    val raw = x.toString + y.toString + (x*y).toString
    raw.sorted == "123456789"
  }

  val a = for {
    x <- 1 to 9
    y <- 1234 to 9876
    if fil(x, y)
  } yield (x, y)
  val b = for {
    x <- 12 to 98
    y <- 123 to 987
    if fil(x, y)
  } yield (x, y)
  assert(b.contains((39, 186)))
  println((a ++ b))
  println((a ++ b).map{ case (x, y) => x * y }.distinct.sum)
}
