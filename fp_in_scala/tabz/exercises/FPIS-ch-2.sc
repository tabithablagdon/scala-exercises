
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
assert(isSorted(Array(1,2,3), (x: Int, y: Int) => x < y))
assert(isSorted(Array("c","b","a"), (x: String, y: String) => x > y))


/**
  * Exercise 2.3: Partial Application
  *
  * Let’s look at another example, currying, which converts a function f of two arguments into a function of one argument that partially applies f. Here again there’s only one implementation that compiles. Write this implementation.
  *
  * def curry[A,B,C](f: (A, B) => C): A => (B => C)
  *
  */

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a,b)


/**
  * Exercise 2.4: Partial Application
  *
  * Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right, A => (B => C) can be written as A => B => C.
  *
  * def uncurry[A,B,C](f: A => B => C): (A, B) => C
  *
  */

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)


/**
  * Exercise 2.5: Partial Application
  *
  * Implement the higher-order function that composes two functions.
  *
  * def compose[A,B,C](f: B => C, g: A => B): A => C
  *
  */

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))