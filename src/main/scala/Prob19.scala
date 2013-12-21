
import com.github.nscala_time.time.Imports._
import org.joda.time.DateMidnight

object Prob19 {
  //daysOfMonth(1900)(0) is Junuary days of 1900
  def daysOfMonth(year: Int) = Vector(31,
    28 + (if(leapYear(year)) 1 else 0),
    31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  def leapYear(year: Int): Boolean = year match {
    case y if y % 400 == 0 => true
    case y if y % 100 == 0 => false
    case y if y % 4 == 0 => true
    case _ => false
  }

  // yearOfDays(0) is days of 1900
  val yearOfDays: Stream[Int] = Stream.from(1900).map { year => daysOfMonth(year).sum }

  // 0 is Monday
  val headWeekdayOfYear: Stream[Int] = 0 #:: headWeekdayOfYear.zip(yearOfDays).map {
    case (before, day) =>
      (before + day) % 7
  }

  def headWeekdayOfMonth(year: Int): Stream[Int] = {
    val headOfYear = headWeekdayOfYear(year - 1900)
    lazy val result: Stream[Int] = headOfYear #:: result.zip(daysOfMonth(year)).map {
      case (before, day) => (before + day) % 7
    }
    result
  }

  def test() {
    for {
      year <- (1901 to 2000).par
      month <- 1 to 12
      joda = new DateMidnight(year, month, 1)
      my = headWeekdayOfMonth(year)(month - 1) + 1
    } {
      assert(joda.getDayOfWeek == my, "Error: year=%d, month=%d".format(year, month))
    }
  }

  def main(args: Array[String]) {
    assert(leapYear(1904))
    assert(daysOfMonth(1900).sum == 365)
    assert(daysOfMonth(1904).sum == 366)
    assert(daysOfMonth(2000).sum == 366)
    assert(headWeekdayOfMonth(1900).head == 0)
    assert(headWeekdayOfYear(4) == 4)
    assert(headWeekdayOfYear(1) == 1)
    test()
    val headWeekdayOfMonths = for {
      year <- 1901 to 2000
      headOfMonth <- headWeekdayOfMonth(year)
    } yield headOfMonth == 6
    println(headWeekdayOfMonths.count(identity))

    val results = for {
      year <- 1901 to 2000
      month <- 1 to 12
      joda = new DateMidnight(year, month, 1)
    } yield joda.getDayOfWeek == 7
    println(results.count(identity))
  }
}
