
object Prob3 extends App {
  def sqrtl(x: Long) = math.sqrt(x.toDouble).toInt
  val number = 600851475143L
  val sqrt = sqrtl(number)
  val primes = Prime.primes(sqrt)
  println( primes.reverse.filter(number % _ == 0).head )
  val divs = primes.flatMap { i =>
    if(number % i == 0) List(i, number / i) else Nil
  }
  divs.filter{ div =>
    primes.takeWhile(_ <= sqrtl(div)).forall(div % _ != 0)
  }.foreach(println)
}
