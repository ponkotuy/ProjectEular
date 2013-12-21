
object Prob44 {
  def pentagonal(n: Int) = n*(3*n - 1)/2
  val pentagonals = Stream.from(0).map(pentagonal)

  val pentaList = pentagonals.takeWhile(_ < (Int.MaxValue >> 2)).toList
  val pentaSet = pentaList.toSet

  def isPenta(n: Int) =
    pentaSet.contains(n)

  private def exec(goal: Int) = {
    def f(j: Int, k: Int): Option[(Int, Int)] = {
      @inline
      def next(pj: Int, pk: Int) = (pk - pj) match {
        case d if d < goal => f(j, k + 1)
        case d if d > goal => f(j + 1, k)
        case _ => f(j + 1, k + 1)
      }
      if(j == k) return None
      require(j < k)
      val pj = pentaList(j)
      val pk = pentaList(k)
      if(!isPenta(pj + pk)) next(pj, pk)
      else if((pk - pj) != goal) next(pj, pk)
      else Some(j, k)
    }
    f(1, 2)
  }

  def main(args: Array[String]) {
    val result = pentagonals.map{i => println(i); i}.map(exec).flatten.head
    println(result)
  }
}
