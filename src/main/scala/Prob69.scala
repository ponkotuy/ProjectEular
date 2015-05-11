object Prob69 extends App {
  val primes = Prime.primes(100000)
  val result = primes.inits.toStream.reverseMap(_.map(_.toLong).product).takeWhile(_ <= 1000000).last
  println(result)
}
