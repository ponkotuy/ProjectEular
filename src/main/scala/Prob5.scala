
object Prob5 {
  def main(args: Array[String]) {
    val max = 20
    val factorization = new Factorization()
    val facts = (2 to max).map { i => factorization(i) }
    val result = (2 to max).map { i =>
      val num = facts.map { _.count(_ == i) }.max
      Iterator.fill(num)(i).product
    }.product
    println(result)
  }
}
