
object Prob2 extends App {
  val num = 4000000
  val fibonacci: Stream[BigInt] = BigInt(0) #:: BigInt(1) #::
    fibonacci.zip(fibonacci.tail).map { case (x, y) => x + y }
  println(fibonacci.takeWhile(_ < num).filter(_ % 2 == 0).sum)
}
