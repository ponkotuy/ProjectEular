
// object Prob26 {
//   def div(left: Int, right: Int): Stream[Int] = {
//     def f(num: Int): Stream[Int] =
//       (num / right) #:: f(num % right * 10)
//     f(left)
//   }

//   def repeat(str: String): Option[String] = {
//     str.inits.drop(str.length/2).find { it =>
//       str.lastIndexOfSlice(it) == it.length
//     }
//   }

//   def reciprocal(xs: Stream[Int]): Option[String] =
//     Stream.from(2).flatMap { i =>
//       val take = xs.take(i)
//       if(take.forall(_ == 0)) None
//       else repeat(take.mkString.reverse)
//     }.headOption.map(_.reverse)

//   def main(args: Array[String]) = {
//     assert(div(1, 2).take(2) == Stream(0, 5))
//     assert(repeat(div(1, 3).take(3).mkString.reverse) == Some("3"))
//     assert(reciprocal(div(1, 7)) == Some("142857"))
//     assert(reciprocal(div(1, 2)) == None)
//     assert(reciprocal(div(1, 11)).get.length == 2)
//     println((3 to 1000).par.flatMap {
//       it => reciprocal(div(1, it)).map(it -> _)
//     }.maxBy(_._2.length))
//   }
// }

import scala.collection.mutable

object Prob26 {
  def div(left: Int, right: Int): Stream[Int] = {
    def f(num: Int): Stream[(Int, Int)] = {
      val ans = num / right
      val odd = num % right
      (ans, odd) #:: f(num % right * 10)
    }
    val result = f(left)
    Stream.from(1).flatMap { i =>
      val part = result.take(i)
      val last = part.last
      part.init.indexWhere( _._2 == last._2 ) match {
        case -1 => None
        case x => Some(part.map(_._1).slice(x + 1, i))
      }
    }.head
  }

  def main(args: Array[String]) {
    assert(div(1, 2) == Stream(0))
    assert(div(1, 3) == Stream(3))
    assert(div(1, 7) == Stream(1, 4, 2, 8, 5, 7))
    val result = (3 to 999).map(it => it -> div(1, it)).maxBy(_._2.length)._1
    println(result)
  }
}
