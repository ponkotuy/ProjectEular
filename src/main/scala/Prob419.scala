
object Prob419 {
  def lookSay(fst: Int, cnt: Int): String = {
    def f(ns: Array[Char]): Array[Char] = {
      if(ns.isEmpty) return Array.empty[Char]
      val (before, after) = ns.span(_ == ns(0))
      Array[Char](('0' + before.size).toChar, ns(0)) ++ f(after)
    }
    val chars = Stream.iterate(fst.toString.toArray)(f).drop(cnt).head
    new String(chars)
  }

  def main(args: Array[String]) {
    require(lookSay(1, 1) == "11")
    require(lookSay(1, 2) == "21")
    require(lookSay(1, 7) == "1113213211")
    val result = lookSay(1, 40)
    println("A(40)=%d".format( result.count(_ == '1') ))
  }
}
