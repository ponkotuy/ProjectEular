
import scalax.file.Path

object Prob22 {
  def loadFile: Array[String] =
    Path("resources/names.txt", '/').string.split(',').map(_.tail.init)

  def score(str: String): Long = str.map(_ - 'A' + 1L).sum

  def main(args: Array[String]) {
    assert(score("COLIN") == 53L)
    val data = loadFile.sorted
    assert(data.indexOf("COLIN") == 937)
    val result = data.zipWithIndex.map {
      case (name, i) => score(name)*(i.toLong + 1L)
    }.sum
    println(result)
  }
}
