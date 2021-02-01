package basics

object Basics {
  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a == 0) b else
    gcd(b % a, a)
  }

  def lcm(a: Int, b: Int): Int = if (gcd(a,b)!=0) a * b / gcd(a, b) else 0
}
