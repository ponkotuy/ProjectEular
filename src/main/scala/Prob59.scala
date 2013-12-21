
import scalax.file.Path
import scala.collection.IterableLike

object Prob59 {
  val COUNT = 3
  def main(args: Array[String]) {
    val text = load()
    charactorFilter(text)
    val result = decrypt(text, "god")
    println(result.map(_.toInt).sum)
  }

  def load() = {
    val raw = Path("resources/cipher1.txt", '/').string.trim
    raw.split(',').map(_.toInt)
  }

  def charactorFilter(text: Iterable[Int]) {
    val CHARS = Set(32, 33, 38, 39, 40, 41, 44, 46, 59, 63) ++
      (48 to 57) ++ (65 to 90) ++ (97 to 122)
    val splited = splitter(text, COUNT)
    val filtered = splited map { xs =>
      CHARS filter { c =>
        xs forall { it => CHARS.contains(it ^ c) }
      }
    }
    println(filtered)
  }

  def wordCountView(text: Iterable[Int]) {
    val splited = splitter(text, COUNT)
    println(splited)
    val counts = splited.map(counter)
    counts map { it =>
      it.toList.sortBy(_._2).reverse
    } foreach(println)
  }

  def decrypt(text: Iterable[Int], key: String): String = {
    val intKey = key.map(_.toInt)
    val intResult = text.grouped(key.length) flatMap { g =>
      g.zip(key) map { case (x, y) => x ^ y }
    }
    intResult.map(_.toChar).mkString
  }

  // splitter("abcdefg", 3) => ["adg", "be", "cf"]
  private def splitter[T](xs: Iterable[T], count: Int) = {
    val bs = Seq.fill(count)(Iterable.newBuilder[T])
    xs.grouped(count).map(println)
    xs.grouped(count) foreach { group =>
      bs.zip(group) foreach { case (b, x) => b += x }
    }
    bs.map(_.result())
  }

  private def counter[T](xss: Traversable[T]): Map[T, Int] =
    xss.groupBy(identity).mapValues(_.size)

}
