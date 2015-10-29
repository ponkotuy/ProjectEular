
import scala.collection.breakOut
import scalax.io.Resource

object Prob89 extends App {
  assert(Roman.toNumber("IIIIIIIIIIIIIIII") == 16)
  assert(Roman.toNumber("VIIIIIIIIIII") == 16)
  assert(Roman.toNumber("VVIIIIII") == 16)
  assert(Roman.toNumber("XIIIIII") == 16)
  assert(Roman.toNumber("VVVI") == 16)
  assert(Roman.toNumber("XVI") == 16)
  assert(isMinimal("XVI"))
  assert(!isMinimal("VVVI"))
  assert(!isMinimal("IIIIIIIIIIIIIII"))
  assert(!isValid("XIIIIIIIIIII"))
  assert(!isValid("VVIIIIII"))
  assert(isValid("XIIIIII"))
  assert(!isValid("VVVI"))
  assert(isValid("XVI"))

  println(Resource.fromFile("resources/p089_roman.txt").lines().map(diffMinimal).sum)

  def diffMinimal(str: String): Int = {
    val num = Roman.toNumber(str)
    str.length - Roman.fromNumber(num).length
  }

  def isMinimal(str: String): Boolean = {
    val num = Roman.toNumber(str)
    str.length <= Roman.fromNumber(num).length
  }

  def isValid(str: String): Boolean = {
    val counts = str.groupBy(identity).map { case (c, xs) => c.toString -> xs.length }
    counts.forall { case (c, cnt) => !Roman.Ones.contains(c) || cnt < 10 } &&
        counts.forall { case (c, cnt) => !Roman.Fives.contains(c) || cnt < 2 }
  }
}

object Roman {
  val Ones = List("I", "X", "C", "M")
  val Fives = List("V", "L", "D")
  val Fours = Fives.zip(Ones).map { case (five, one) => one + five }
  val Nines = Ones.tail.zip(Ones).map { case (ten, one) => one + ten }
  val TenPowers: Stream[Int] = 1 #:: TenPowers.map(_ * 10)

  val FromChar2 = toNumberTable(Fours, 4) ++ toNumberTable(Nines, 9)
  val FromChar1 = toNumberTable(Ones, 1) ++ toNumberTable(Fives, 5)

  val FromNumber =
    (toStringTable(Ones, 1) ++ toStringTable(Fours, 4) ++ toStringTable(Fives, 5) ++ toStringTable(Nines, 9))
        .sortBy(-_._1)

  private def toNumberTable(xs: Seq[String], base: Int): Map[String, Int] = {
    xs.zip(TenPowers).map { case (x, tens) => x -> base * tens}(breakOut)
  }

  private def toStringTable(xs: Seq[String], base: Int): List[(Int, String)] = {
    xs.zip(TenPowers).map { case (x, tens) => base * tens -> x}(breakOut)
  }

  def toNumber(str: String): Int = {
    if(str.isEmpty) return 0
    FromChar2.get(str.take(2)).fold {
      FromChar1(str.take(1)) + toNumber(str.drop(1))
    } { num =>
      num + toNumber(str.drop(2))
    }
  }

  def fromNumber(num: Int): String = {
    FromNumber.find(_._1 <= num).fold("") { case (n, str) =>
      str + fromNumber(num - n)
    }
  }
}
