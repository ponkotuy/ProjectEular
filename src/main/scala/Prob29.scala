
import scala.collection.mutable

object Prob29 extends App {
  val set = mutable.Set[BigInt]()
  (2 to 100).foreach { i =>
    val b = BigInt(i)
    Stream.fill(98)(b).scan(b*b)(_*_).foreach { set += _ }
  }
  println(set.size)
}
