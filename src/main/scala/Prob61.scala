/**
 * User: yosuke
 * Date: 13/10/15
 * Time: 5:01
 */
object Prob61 {
  def main(args: Array[String]) {
    val polygonals = ((3 to 8) map { i =>
        i -> Stream.from(1)
          .map(polygonal(i))
          .dropWhile(_ < 1000).takeWhile(_ < 10000)
          .toSet
    }).toMap
    val rawResult = polygonals(3) flatMap { x =>
      f(polygonals, List.range(4, 9), x%100).map(list => x :: list)
    }
    val result = rawResult.filter { x =>
      x.head / 100 == x.last % 100
    }
    println(result)
  }

  def f(polygonals: Map[Int, Set[Int]], gonals: List[Int], x: Int): List[List[Int]] = {
    if(gonals.isEmpty) return List(Nil)
    gonals flatMap { g =>
      val rest = gonals.filter(_ != g)
      polygonals(g).filter(_ / 100 == x) flatMap { y =>
        f(polygonals, rest, y % 100).map(list => y :: list)
      }
    }
  }

  def polygonal(gonal: Int)(n: Int): Int =
    n*((gonal - 2)*n + (4 - gonal))/2

}
