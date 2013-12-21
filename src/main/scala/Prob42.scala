
import scalax.file.Path

object Prob42 {
  // 0, 1, 3 ...
  val triangles: Stream[Int] =
    Stream.from(0).map { i => i*(i + 1) >> 1 }

  def triangleWord(word: String): Option[Int] = {
    val value = word.toLowerCase.map {
      "abcdefghijklmnopqrstuvwxyz".indexOf(_) + 1
    }.sum
    triangles.dropWhile(_ < value).head == value match {
      case true => Some(triangles.indexOf(value))
      case false => None
    }
  }

  private def loadFile(fName: String = "resources/words.txt"): Array[String] = {
    val str = Path(fName, '/').string
    str.split(",").map { _.replace("\"", "") }
  }

  def main(args: Array[String]) {
    assert(triangles.slice(1, 11) ==
      Stream(1, 3, 6, 10, 15, 21, 28, 36, 45, 55))
    assert(triangleWord("SKY") == Some(10))
    println(loadFile().map(triangleWord).flatten.length)
  }
}
