package basics

object Collections {
  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSum(nums: Array[Int]): Array[Int] = {
    // alternative implementation
    //    nums.map(x => nums.slice(0, nums.indexOf(x) + 1).sum)
    nums.scanLeft(0)(_ + _).slice(1, nums.length + 1)
  }

  // https://leetcode.com/problems/shuffle-the-array
  def shuffle(nums: Array[Int], n: Int): Array[Any] = {
    (nums zip nums.slice(n, nums.length)).flatMap(_.productIterator.toList)
  }

  // https://leetcode.com/problems/richest-customer-wealth
  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
    points.map(_.head).sorted.sliding(2).map(a => a.last - a.head).max
  }

  def count(s: String): List[(Char, Int)] = {
    val l = "((.)\\2*)".r.findAllIn(s).toList
    l.map(_ (0)) zip l.map(_.length)
  }

  def sortConsideringEqualValues[T](map: Map[T, Int]): Map[Set[T], Int] = {
    map.groupBy(_._2).map(x => (x._2.keys.toSet, x._2.values.head)).toSeq.sortBy(_._2).toMap
  }
}
