
import scala.collection.mutable

object Prob14 {

  def createCache(max: Int): scala.collection.Map[Int, Int] = {
  val cache = mutable.Map[Int, Int]()
    (1 to max).foreach { num =>
      if(!cache.contains(num)) {
        val own = collatz(num)
        cache(num) = own
        Stream.iterate((num*2, own + 1)) { case (n, cnt) =>
            if(cache.getOrElse(n, Int.MaxValue) > cnt) cache(n) = cnt
            (n*2, cnt + 1)
        }.takeWhile(_._1 < max).to
      }
    }
    cache
  }


  def collatz(num: Long): Int =  {
    Stream.iterate(num) { n =>
      require(n > 0)
      if ( (n & 1) == 0 ) n >> 1
      else 3*n + 1
    }.takeWhile(_ != 1).size + 1
  }

  def main(args: Array[String]) {
    val cache = createCache(1000000)
    println(cache.maxBy(_._2))
  }
}
