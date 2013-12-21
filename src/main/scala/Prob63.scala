/**
 * User: yosuke
 * Date: 13/10/16
 * Time: 22:08
 */
object Prob63 extends App {
  val result = Stream.from(1) map { x =>
    val result = Range(9, 0, -1) count { y =>
      pow(y, x).toString().size == x
    }
    x -> result
  }
  println(result.takeWhile(_._2 > 0).map(_._2).sum)

  def pow(x: Int, n: Int): BigInt =
    (1 to n).map(_ => BigInt(x)).product
}
