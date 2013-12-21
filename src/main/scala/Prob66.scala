
/**
 * User: yosuke
 * Date: 13/10/21
 * Time: 0:38
 */
object Prob66 {
  def main(args: Array[String]) {
    val squares = (1L to 100000L).map(i => (i*i, i)).toMap
    val result = (2L to 1000L).filterNot(squares.contains) map { d =>
      val opts = Stream.from(1) flatMap { y =>
        squares.get(d*y*y + 1L)
      }
      d -> opts.head
    }
    val resultMap = result.toMap
    assert(resultMap.filter(_._1 < 8).maxBy(_._2) == (5, 9))
    println(resultMap.maxBy(_._2))
  }
}
