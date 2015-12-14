
object Prob47 {
  val pFact = new Factorization()

  def isConsecutive(n: Int): Int = pFact(n).distinct.length

  def main(args: Array[String]) {
    assert(Array(14, 15).map(isConsecutive).forall(_ == 2))
    assert(Array(644, 645, 646).map(isConsecutive).forall(_ == 3))
    val result = Stream.from(14)
      .filter(isConsecutive(_) == 4)
      .sliding(4)
      .filter { xs => xs.last - xs.head == 3 }
      .next
    println(result)
  }
}
