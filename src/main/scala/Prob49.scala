
object Prob49 {
  val primes = Prime.set(9999)

  // Prime Permutations
  def primePerm(prime: Int): List[(Int, Int, Int)] = {
    val perms = prime.toString
      .permutations
      .filter(_(0) != '0')
      .map(_.toInt)
      .filter(primes.contains)
      .toList.sorted
    for {
      x <- perms
      y <- perms
      if x < y
      z = y + (y - x)
      if perms.contains(z)
    } yield (x, y, z)
  }

  def main(args: Array[String]) {
    assert(primePerm(1487) == List((1487, 4817, 8147)))
    primes.filter(_ >= 1000)
      .map(primePerm)
      .filter(_.nonEmpty)
      .foreach(println)
  }
}
