
object Prob9 extends App {
  val result = for {
    a <- 1 to 334
    b <- a to 500
    c = 1000 - a - b
    if a*a + b*b == c*c
  } yield a*b*c
  println(result.head)
}
