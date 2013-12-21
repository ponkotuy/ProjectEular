
/**
 * User: yosuke
 * Date: 13/10/15
 * Time: 13:32
 */
object Prob62 {
  val MAX = 1000
  val cubics = Stream.from(0).map(cubic)
  val cubicSet = cubics.takeWhile(_ <= cubic(MAX)).toSet

  def main(args: Array[String]) {
    val digitSorted = cubics map { n => n -> splitNum(n).sorted }
    val sampleResult = digitSorted.dropWhile(_._2.size < 8)
      .takeWhile(_._2.size == 8)
      .count(_._2 == List(0, 1, 2, 3, 4, 5, 6, 6))
    println(sampleResult)
    assert(sampleResult == 3)
    (8 to 12) map { dig =>
      val xs = digitSorted.dropWhile(_._2.size < dig).takeWhile(_._2.size == dig)
      val results = xs.groupBy(_._2).values map { xs =>
        xs.map(_._1).min -> xs.size
      }
      results.filter(_._2 > 3).foreach(println)
    }
  }

  def cubic(n: Int): Long = n.toLong*n*n

  def permCubicCount(n: Long): Int = {
    val xs = splitNum(n).reverse
    xs.permutations count { x =>
      val long = joinNum(x)
      (long >= n) && isCubic(long)
    }
  }

  def splitNum(n: Long): List[Int] = {
    val div = n / 10
    val rem = n % 10
    if(div == 0 && rem == 0) Nil
    else rem.toInt :: splitNum(div)
  }

  def joinNum(xs: List[Int]): Long = {
    if(xs.isEmpty) return 0
    val head :: tail = xs
    if(tail.isEmpty) xs.head
    else head + joinNum(tail)*10
  }

  def isCubic(n: Long): Boolean =
    cubicSet.contains(n)
}
