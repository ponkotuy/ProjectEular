
object Prob38 {
  val BASE_SET = (1 to 9).map('0' + _).toSet
  def isPandigital(num: Int): Boolean = {
    val stream = (1 to 9).toStream.map { i =>
      (1 to i).map(_*num).map(_.toString).mkString
    }
    val str = stream.dropWhile(_.length < 9).head
    str.length == 9 && str.toSet == BASE_SET
  }

  def main(args: Array[String]) {
    assert(isPandigital(192))
    assert(isPandigital(9))
    println((193 to 9876).filter(isPandigital).last)
  }
}
