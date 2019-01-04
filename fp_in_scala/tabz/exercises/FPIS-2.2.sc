
/**
  * Exercise 2.2: Polymorphic Function
  *
  * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
  *
  * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
  *
  */

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(curr: Int): Boolean = {
    if (curr >= as.length - 1) true
    else if (!ordered(as(curr), as(curr + 1))) false
    else loop(curr + 1)
  }

  loop(0)
}

/**
  * Test cases
  */
// isSorted(Array(1,2,3), (x: Int, y: Int) => x < y)
// => true

// isSorted(Array("c","b","a"), (x: String, y: String) => x > y)
// => true
