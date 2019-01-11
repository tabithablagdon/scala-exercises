import List._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(_, tail) => tail
  }

  /**
    * Generalize tail to the function drop, which removes the first n elements from a list.
    * Note that this function takes time proportional only to the number of elements
    * being dropped—we don't need to make a copy of the entire List.
    */

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case n if n <= 0 => l
    case _ => drop(tail(l), n-1)
  }

  /**
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, _) if f(h) => dropWhile(tail(l), f)
    case _ => l
  }

  /**
    *
    * Implement a function, init, that returns a List consisting of all,
    * but the last element of a List. So, given List(1,2,3,4), init will
    * return List(1,2,3). Why can't this function be implemented in
    * constant time like tail?
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, tail) => Cons(h, init(tail))
  }

  /**
    * Can product, implemented using foldRight, immediately halt the recursion
    * and return 0.0 if it encounters a 0.0? Why or why not? Consider how any
    * short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we'll return to in chapter 5.
    */

  // No, the operation needs to run through the entire list (tail recursion finishes)
  //

  /**
    * See what happens when you pass Nil and Cons themselves to foldRight,
    * like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    */
  
  // Return the same list

  /**
    * Compute the length of a list using foldRight
    */
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, len) => len + 1)

  /**
    * Our implementation of foldRight is not tail-recursive
    * and will result in a StackOver-flowError for large lists
    * (we say it's not stack-safe).
    * Convince yourself that this is the case, and then write another
    * general list-recursion function, foldLeft, that is tail-recursive,
    * using the techniques we discussed in the previous chapter.
    * */

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  /**
    * Write sum, product, and a function to compute the length of a list using foldLeft.
  */
  def sumFL(l: List[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def productFL(l: List[Int]): Int = l match {
    case Nil => 0
    case _ => foldLeft(l, 1)(_ * _)
  }

  def lengthFL[A](l: List[A]): Int =
    foldLeft(l, 0)((len, _) => len + 1)

  /**
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    */
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((y, x) => Cons(x, y))

  def reverseFR[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => foldRight(reverse(t), Cons(h, Nil))((x, y) => Cons(x, y))
  }

  /**
    * Implement append in terms of either foldLeft or foldRight.
    */
  def append[A](l: List[A], item: A): List[A] =
    foldRight(l, Cons(item, Nil))(Cons(_, _))

  /**
    * Implement concat in terms of either foldLeft or foldRight.
    */
  def concat[A](l: List[A], l2: List[A]): List[A] =
    foldRight(l, l2)(Cons(_, _))

  /**
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    */

  def flatten[A](as: List[List[A]]): List[A] =
    foldRight(as, Nil: List[A])(concat)


  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case _ => Nil
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Cons(h, t) if f(h) => Cons(h, filter(t)(f))
    case Cons(_, t) => filter(t)(f)
    case _ => Nil
  }

  /**
    *
    * Write a function flatMap that works like map except that the function
    * given will return a list instead of a single result, and that list
    * should be inserted into the final resulting list. Here is its
    * signature:
    *
    * def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
    *
    * For instance, flatMap(List(1,2,3))(i => List(i,i))
    * should result in List(1,1,2,2,3,3).
    *
    */

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => foldRight(f(h), flatMap(t)(f))((x, y) => Cons(x, y))
  }
}


val testList = List(1, 2, 3, 4)
val testList2 = List(1)
val testEmptyList = Nil

//tail tests
reportTestResult("tail on empty list should return empty list", tail(testEmptyList) == Nil)
reportTestResult("tail should return list tail", tail(testList) == List(2, 3, 4))

//drop tests
reportTestResult("drop should drop specified elements", drop(testList, 2) == List(3, 4))
reportTestResult("drop should drop specified elements", drop(testList2, 2) == Nil)
reportTestResult("drop should work on empty list", drop(testEmptyList, 2) == Nil)
reportTestResult("drop should drop no items", drop(testList2, 0) == testList2)
reportTestResult("drop should drop no items", drop(testList, -1) == testList)

// dropWhile tests
reportTestResult("dropWhile should transform list", dropWhile(testList, (x: Int) => x < 2) == List(2, 3, 4))
reportTestResult("dropWhile should transform list", dropWhile(testList, (x: Int) => x < 4) == List(4))
reportTestResult("dropWhile should transform list", dropWhile(testList2, (x: Int) => x < 2) == Nil)
reportTestResult("dropWhile should work on empty list", dropWhile(testEmptyList, (x: Int) => x < 2) == Nil)

//init tests
reportTestResult("init", init(testList) == List(1, 2, 3))
reportTestResult("init", init(testList2) == Nil)
reportTestResult("init", init(testEmptyList) == Nil)

// length tests
reportTestResult("length should return list length", length(testList) == 4)
reportTestResult("length should return list length ", length(testList2) == 1)
reportTestResult("length should 0 for empty list", length(testEmptyList) == 0)

//foldLeft tests
reportTestResult("sum list of ints", sumFL(testList) == 10)
reportTestResult("sum list of ints", sumFL(testList2) == 1)
reportTestResult("sum empty list", sumFL(testEmptyList) == 0)

//productFL tests
reportTestResult("product list of ints", productFL(testList) == 24)
reportTestResult("product list of ints", productFL(testList2) == 1)
reportTestResult("product empty list", productFL(testEmptyList) == 0)

//length tests
reportTestResult("length should return list length", lengthFL(testList) == 4)
reportTestResult("length should return list length ", lengthFL(testList2) == 1)
reportTestResult("length should 0 for empty list", lengthFL(testEmptyList) == 0)

//reverse tests
reportTestResult("reverse should reverse a list", reverse(testList) == List(4, 3, 2, 1))
reportTestResult("reverse should reverse a list", reverse(testList2) == List(1))
reportTestResult("reverse should return Nil if empty list", reverse(testEmptyList) == Nil)

reportTestResult("reverseFR should reverse a list", reverseFR(testList) == List(4, 3, 2, 1))
reportTestResult("reverseFR should reverse a list", reverseFR(testList2) == List(1))
reportTestResult("reverseFR should return Nil if empty list", reverseFR(testEmptyList) == Nil)

//concat tests
reportTestResult("concat should transform a list", concat(testList, testList2) == List(1, 2, 3, 4, 1))
reportTestResult("concat should transform a list", concat(testList2, testEmptyList) == List(1))

//append tests
reportTestResult("append should append to list", append(testList, 5) == List(1, 2, 3, 4, 5))
reportTestResult("append should append to list", append(testEmptyList, 3) == List(3))

//map tests
reportTestResult("map should transform a list", map(testList)(x => x * 2) == List(2, 4, 6, 8))
reportTestResult("map should transform a list", map(testList2)(x => x * 2) == List(2))

//filter tests
reportTestResult("filter should transform a list", filter(testList)(x => x > 2) == List(3, 4))
reportTestResult("filter should transform a list", filter(testList2)(x => x > 2) == Nil)

//flatten tests
reportTestResult("flatten should flatten a list of lists", flatten(List(testList, testList2)) == List(1, 2, 3, 4, 1))
reportTestResult("flatten should transform a list of lists", flatten(List(testList2, testEmptyList)) == List(1))

//flatMap tests
reportTestResult("flatMap should transform a list", flatMap(testList)(i => List(i,i)) == List(1, 1, 2, 2, 3, 3, 4, 4))
reportTestResult("flatMap should transform a list", flatMap(testList2)(i => List(i,i)) == List(1, 1))

def reportTestResult(testName: String, assertion: Boolean): Unit = assertion match {
  case true =>
    println(s"PASS: $testName")
  case _ =>
    println(s"FAIL: $testName")
}
