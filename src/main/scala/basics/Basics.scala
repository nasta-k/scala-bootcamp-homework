package basics

object Basics {
  @scala.annotation.tailrec
  def gcd(a: Int, b: Int): Int = {
    if (a == 0) return b
    gcd(b % a, a)
  }

  def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)
}
