
object Prob39 extends App {
  def triCount(p: Int): Int = {
    val result = for {
      x <- 1 to p/3
      y <- x to p/2
      z = p - x - y
      if x*x + y*y == z*z
    } yield (x, y, z)
    result.size
  }
  assert( triCount(120) == 3 )
  println(Range(12, 1000).maxBy(triCount))
}
