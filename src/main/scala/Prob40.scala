
object Prob40 extends App {
  val result = (0 to 250000).map(_.toString).mkString
  assert(result(12) - '0' == 1)
  println(
    List(1, 10, 100, 1000, 10000, 100000, 1000000).map(result.apply).map(_ - '0').product
  )
}
