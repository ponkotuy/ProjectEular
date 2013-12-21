
object Prob10 extends App {
  val primes = Prime.primes(2000000)
  println(primes.map(_.toLong).sum)
}
