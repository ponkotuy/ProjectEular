
object Prob16 extends App {
  val result = Iterator.fill(1000)(BigInt(2)).product
  val sum = Stream.iterate((result, 0)){
    case (rest, ans) => (rest/10, ans + (rest%10).toInt)
  }.dropWhile {
    case (x, _) if x == BigInt(0) => false
    case _ => true
  }.head._2
  println(sum)
}
