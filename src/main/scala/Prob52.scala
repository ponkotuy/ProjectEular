
object Prob52 {
  private def intToList(n: Int): List[Int] = n match {
    case 0 => Nil
    case x => (x % 10) :: intToList(x/10)
  }

  private def listToInt(xs: List[Int]): Int = xs match {
    case Nil => 0
    case x :: xs => x + 10*listToInt(xs)
  }

  def permutedMultiple(n: Int): Int = {
    val nlist = intToList(n)
    Stream.from(1).takeWhile { i =>
      val x = i*n
      val xlist = intToList(x)
      nlist.sorted == xlist.sorted
    }.length
  }

  def main(args: Array[String]) {
    assert(permutedMultiple(125874) == 2)
    val result = Stream.from(125875)
      .filter(permutedMultiple(_) == 6)
    println(result.head)
  }
}
