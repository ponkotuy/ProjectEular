import scala.io.Source
import scala.collection.breakOut

object Prob79 extends App {
  assert(digits(1000) == List(1, 0, 0, 0))
  val logs = KeyLog.fromFile(Source.fromFile("resources/p079_keylog.txt"))
  val result = Iterator.from(1000).filter { cand =>
    logs.forall { log => check(digits(cand), log) }
  }.next()
  println(result)

  def check(cand: List[Int], log: List[Int]): Boolean = log match {
    case y :: ys =>
      cand match {
        case `y` :: xs => check(xs, ys)
        case _ :: xs => check(xs, log)
        case Nil => false
      }
    case Nil => true
  }

  def digits(num: Int): List[Int] = {
    def f(n: Int): List[Int] = if(n < 10) List(n) else n % 10 :: f(n / 10)
    f(num).reverse
  }
}

object KeyLog {
  def fromFile(source: Source): List[List[Int]] = {
    source.getLines().map { line =>
      line.map(_ - '0')(breakOut): List[Int]
    }.toList
  }
}
