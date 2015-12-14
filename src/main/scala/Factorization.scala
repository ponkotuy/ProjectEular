
class Factorization() {
  def apply(x: Int): List[Int] = {
    def f(rest: Int, now: Int): List[Int] = {
      if(rest == 1) return Nil
      if(rest%now == 0) now :: f(rest/now, now)
      else f(rest, now + 1)
    }
    f(x, 2)
  }
}
