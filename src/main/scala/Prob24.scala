
// object Prob24 {
//   def next(ns: Vector[Int]): Vector[Int] = {
//     ns.reverse.flatMap { n =>
//       val (nomove, move) = ns.reverse.span(_ != n)
//       if(move.isEmpty) None
//       else {
//         val (before, after) = move.drop(1).span(_ < n)
//         println(nomove, before, after)
//         Some(after.reverse ++ Vector(n) ++ before.reverse ++ nomove.reverse)
//       }
//     }.head
//   }
//   def main(args: Array[String]) {
//     println(next(Vector(0, 1, 2)))
//     assert(next(Vector(0, 1, 2)) == Vector(0, 2, 1))
//   }
// }

object Prob24 extends App {
  def digitalize(xs: Traversable[Int]): String =
    xs.mkString

  val permutate = (0 to 9).permutations.toVector.sortBy(digitalize)

  println(digitalize(0 to 9))
  assert(digitalize(0 to 9) == "0123456789")
  println(permutate.take(10))
  assert(permutate(0) == (0 to 9))
  println(permutate(999999))
}
