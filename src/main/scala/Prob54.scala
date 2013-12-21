
import scalax.file.Path
import scalax.io.LongTraversable

object Prob54 {
  /**
    * @param value: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11(J), 12(Q), 13(K), 14(A)
    */
  case class Card(suit: Char, value: Int)
  object Card {
    def apply(str: String): Card = {
      val value = str(0) match {
        case 'T' => 10
        case 'J' => 11
        case 'Q' => 12
        case 'K' => 13
        case 'A' => 14
        case x => x - '0'
      }
      Card(str(1), value)
    }
  }

  object Cards {
    def apply(s1: String, s2: String, s3: String, s4: String, s5: String): List[Card] =
      List(Card(s1), Card(s2), Card(s3), Card(s4), Card(s5))
  }

  def strength(hand: List[Card]): Int = {
    require(hand.length == 5)

    def strength(cards: List[Int]): Int = cards match {
      case Nil => 0
      case x :: xs => (x - 1) + 14*strength(xs)
    }

    val valueStrength = {
      val priority = hand.groupBy(_.value)
        .mapValues(_.length).toList
        .sortBy { case (value, length) => length*14 + value }
        .map(_._1)
      strength(priority)
    }
    pokerHands(hand)*1000000 + valueStrength
  }

  def pokerHands(hand: List[Card]): Int = {
    require(hand.length == 5)

    val flush = hand.map(_.suit).distinct.length == 1
    val strait = {
      val vals = hand.map(_.value).distinct.sorted
      vals.length == 5 && (vals.max - vals.min) == 4
    }
    val kinds =
      hand.groupBy(_.value).map(_._2.length).filter(_ > 1).toList.sorted.reverse
    (flush, strait, kinds) match {
      case (true, true, _) => hand.map(_.value).max match {
        case 14 => 9 // Royal Strait Flush
        case _ => 8  // Strait Flush
      }
      case (_, _, List(4)) => 7    // Four of a Kind
      case (_, _, List(3, 2)) => 6 // Full House
      case (true, _, _) => 5       // Flush
      case (_, true, _) => 4       // Strait
      case (_, _, List(3)) => 3    // Three of a Kind
      case (_, _, List(2, 2)) => 2 // Two Pairs
      case (_, _, List(2)) => 1    // One Pair
      case _ => 0
    }
  }

  def reading(fName: String = "resources/poker.txt"): LongTraversable[List[Card]] = {
    val path = Path(fName, '/')
    path.lines().map { line =>
      line.split(' ').map(Card.apply).toList
    }
  }

  def main(args: Array[String]) {
    test1()
    if(args.nonEmpty) debug(args(0).toInt)
    val xs = reading().toStream
    val result = xs.map { x =>
      val Array(player1, player2) = x.grouped(5).toArray
      strength(player1) compare strength(player2)
    }
    test2(result)
    println(result.count(_ > 0))
  }

  def test1() {
    assert(pokerHands(Cards("5H", "5C", "6S", "7S", "KD")) == 1)
    assert(pokerHands(Cards("5D", "8C", "9S", "JS", "AC")) == 0)
    assert(pokerHands(Cards("2D", "9C", "AS", "AH", "AC")) == 3)
    assert(pokerHands(Cards("3D", "6D", "7D", "TD", "QD")) == 5)
    assert(strength(Cards("5H", "5C", "6S", "7S", "KD")) <
      strength(Cards("2C", "3S", "8S", "8D", "TD"))
    )
    assert(strength(Cards("5D", "8C", "9S", "JS", "AC")) >
      strength(Cards("2C", "5C", "7D", "8S", "QH"))
    )
    assert(strength(Cards("2D", "9C", "AS", "AH", "AC")) <
      strength(Cards("3D", "6D", "7D", "TD", "QD"))
    )
    assert(strength(Cards("4D", "6S", "9H", "QH", "QC")) >
      strength(Cards("3D", "6D", "7H", "QD", "QS"))
    )
    assert(strength(Cards("2H", "2D", "4C", "4D", "4S")) >
      strength(Cards("3C", "3D", "3S", "9S", "9D"))
    )
  }

  def test2(xs: Traversable[Int]) {
    val path = Path("resources/answer54.txt", '/')
    val answer = path.lines().map { _ match {
      case "win" => 1
      case "draw" => 0
      case "lose" => -1
    }}.toList
    answer.zip(xs.toList).zipWithIndex.foreach { case ((ans, result), i) =>
      if(ans != result) println(s"Mismatch: ${i}")
    }
  }

  def debug(num: Int) {
    val x = reading()(num)
    val Array(player1, player2) = x.grouped(5).toArray
    println(pokerHands(player1), strength(player1))
    println(pokerHands(player2), strength(player2))
    sys.exit(0)
  }
}
