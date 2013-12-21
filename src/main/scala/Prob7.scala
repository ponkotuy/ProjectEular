
object Prob7 extends App {
  val primes = Prime.primes(200000) // nまでの素数の数はn/ln(n)にほぼ等しい
  println(primes(10000))
}
