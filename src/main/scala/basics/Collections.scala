package basics

object Collections {
  def runningSum(nums: Array[Int]): Array[Int] = {
    // alternative implementation
    //    nums.map(x => nums.slice(0, nums.indexOf(x) + 1).sum)
    nums.scanLeft(0)(_ + _).slice(1, nums.length + 1)
  }

  def shuffle(nums: Array[Int], n: Int): Array[Any] = {
    (nums zip nums.slice(n, nums.length)).flatMap(_.productIterator.toList)
  }

  def maximumWealth(accounts: Array[Array[Int]]): Int = {
    accounts.map(_.sum).max
  }

  def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    candies.map(_ + extraCandies >= candies.max)
  }

  // Implement scanLeft (not using scans ofc)
  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = {
    for (el <- list) {
      println(scanLeft(f(zero, el))(list)(f))
      println(f(el, zero))
    }
    list.map(x => f(zero, x))
  }

  def count(s: String): List[(Char, Int)] = {
    val l = "((.)\\2*)".r.findAllIn(s).toList
    l.map(_ (0)) zip l.map(_.length)
  }

  def sortConsideringEqualValues[T](map: Map[T, Int]): Map[Set[T], Int] = {
    map.groupBy(_._2).map(x => (x._2.keys.toSet, x._2.values.head)).toSeq.sortBy(_._2).toMap
  }

}
