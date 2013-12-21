
object Prob25 extends App {
  val fibonacci: Stream[BigInt] = BigInt(0) #:: BigInt(1) #::
    fibonacci.zip(fibonacci.tail).map(it => it._1 + it._2)

  assert(fibonacci(5) == BigInt(5))
  assert(fibonacci(12) == BigInt(144))
  println( fibonacci.indexWhere(_.toString.length >= 1000) )
}
