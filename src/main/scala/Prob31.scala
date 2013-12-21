
object Prob31 extends App {
  val sum = 200
  val result = for {
    p200 <- 0 to sum/200
    r200 = sum - p200*200
    p100 <- 0 to r200/100
    r100 = r200 - p100*100
    p50 <- 0 to r100/50
    r50 = r100 - p50*50
    p20 <- 0 to sum/20
    r20 = r50 - p20*20
    p10 <- 0 to r20/10
    r10 = r20 - p10*10
    p5 <- 0 to r10/5
    r5 = r10 - p5*5
    p2 <- 0 to r5/2
    r2 = r5 - p2*2
    p1 = r2
  } yield (p1, p2, p5, p10, p20, p50, p100, p200)
  println(result.length)
}
