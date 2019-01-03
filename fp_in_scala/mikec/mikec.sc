/**
  * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
  * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
  */

def isSorted[A](as: Seq[A], ordered: (A,A) => Boolean): Boolean =
  as.sliding(2).foldLeft(true) {
    case(sortedSoFar, Seq(a, b)) => sortedSoFar && ordered(a,b)
  }

def isSortedR[A](as: Seq[A], ordered: (A,A) => Boolean): Boolean = {
  as match {
    case Seq() => true
    case Seq(_) => true
    case Seq(a, b, remaining@_*) => ordered(a,b) && isSortedR(remaining, ordered)
  }
}

assert(isSorted(Array(1,2,3,4), (a:Int,b:Int) => a <= b))
assert(isSorted(Array(4,3,2,1), (a:Int,b:Int) => a >= b))
assert(!isSorted(Array(4,3,4), (a:Int,b:Int) => a <= b))
assert(!isSorted(Array('a','b','c'), (a:Char,b:Char) => a <= b))
//assert(!isSorted(Array('c','b','a'), (a:Char,b:Char) => a >= b))
//assert(!isSorted(Array('c','b','c'), (a:Int,b:Int) => a >= b))

assert(isSortedR(Array(1,2,3,4), (a:Int,b:Int) => a <= b))
assert(isSortedR(Array(4,3,2,1), (a:Int,b:Int) => a >= b))
assert(!isSortedR(Array(4,3,4), (a:Int,b:Int) => a <= b))
assert(!isSortedR(Array('a','b','c'), (a:Char,b:Char) => a <= b))
//assert(!isSortedR(Array('c','b','a'), (a:Char,b:Char) => a >= b))
//assert(!isSortedR(Array('c','b','c'), (a:Int,b:Int) => a >= b))

/**
  * Let's look at another example, currying,9 which converts a function f of two arguments into a function of one argument that partially applies f. Here again there's only one implementation that
  * compiles. Write this implementation.
  * def curry[A,B,C](f: (A, B) => C): A => (B => C)
  */
def curry[A,B,C](f: (A, B) => C): A => B => C =
  (a: A) => (b: B) => f(a, b)

/**
  * Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right, A => (B => C) can be written as A => B => C.
  * def uncurry[A,B,C](f: A => B => C): (A, B) => C
  */

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

/**
  * Implement the higher-order function that composes two functions.
  * def compose[A,B,C](f: B => C, g: A => B): A => C
  */

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))
