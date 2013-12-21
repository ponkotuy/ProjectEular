
import scala.collection.mutable

object Prob15 extends App {
  def createCache(max: Int) = {
    val cache = mutable.Map[(Int, Int), Long]()
    (0 to max).foreach { i =>
      (i to max).foreach { j =>
        cache((i, j)) =
          if(i == 0) 1
          else if(i == j) cache(i - 1, j)*2
          else cache(i - 1, j) + cache(i, j - 1)
      }
    }
    cache
  }
  val cache = createCache(20)
  println(cache(20, 20))
}
