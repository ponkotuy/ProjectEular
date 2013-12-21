
object Prob68 {
  def main(args: Array[String]) {
    require(nGonRingMax(3).value == BigInt("432621513"))
  }

  def nGonRingMax(size: Int): Ring = {
    def f(rest: Set[Int], order: List[Int], ring: Ring): Seq[Ring] = {
      rest.toSeq.sorted.reverse.flatMap { i =>
        val ring_ = ring.updated(order.head, i)
        if(ring_.valid) Some(ring_)
        else None
      }
    }
    val order = List(1, size + 1, size + 2) ++
      (2 to size - 1).flatMap(i => List(i, i + size + 1)) ++
      List(size)
    println(order)
    f((1 to size*2).toSet, order, Ring(size)).max
  }
}

case class Ring(outer: Array[Int], inner: Array[Int]) {
  val size = math.min(outer.length, inner.length)/2

  def apply(i: Int): Int = {
    require(i < size * 2)
    if(i < size) outer(i)
    else inner(i - size)
  }

  def updated(i: Int, x: Int): Ring = {
    require(i < size * 2)
    if(i < size) {
      val o = outer.updated(i, x)
      val in = inner.clone()
      Ring(o, in)
    } else {
      val o = outer.clone()
      val in = inner.updated(i - size, x)
      Ring(o, in)
    }
  }

  def lines: Array[(Int, Int, Int)] = (inner, outer, outer.tail ++ Array(outer.head)).zipped.toArray

  def valid: Boolean = {
    val totals = lines.filterNot { case (x, y, z) => x == 0 || y == 0 || z == 0 }
      .map { case (x, y, z) => x + y + z }
    if(totals.isEmpty) true
    else totals.tail.forall(_ == totals.head)
  }

  def value: BigInt = {
    val list = lines.toList.flatMap { case (x, y, z) => List(x, y, z) }
    list.foldLeft[BigInt](0)(_ * 10 + BigInt(_))
  }
}

object Ring {
  def apply(size: Int): Ring =
    Ring(Array.fill(size)(0), Array.fill(size)(0))

  implicit def ringOrdering: Ordering[Ring] =
    Ordering.fromLessThan(_.value < _.value)
}
