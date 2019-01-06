import List._

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => l
    case Cons(h, tail) => tail
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
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  /**
    *
    * Implement a function, init, that returns a List consisting of all,
    * but the last element of a List. So, given List(1,2,3,4), init will
    * return List(1,2,3). Why can't this function be implemented in
    * constant time like tail?
    */
  def init[A](l: List[A]): List[A] = ???


  /**
    * Can product, implemented using foldRight, immediately halt the recursion
    * and return 0.0 if it encounters a 0.0? Why or why not? Consider how any
    * short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we'll return to in chapter 5.
    */


  /**
    * See what happens when you pass Nil and Cons themselves to foldRight,
    * like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    */

  /**
    * Compute the length of a list using foldRight
    */
  def length[A](as: List[A]): Int = ???

  /**
    * Our implementation of foldRight is not tail-recursive and will result in a StackOver -
    * flowError for large lists (we say it's not stack-safe).
    * Convince yourself that this is the case, and then write another general list-recursion function,
    * foldLeft, that is tail-recursive, using the techniques we discussed in the previous chapter.
    * Here is its
    * */

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = ???

  /**
    * Write sum, product, and a function to compute the length of a list using foldLeft.
  */
  def sum: Int= ???

  def product: Int = ???

  def length: Int = ???

  /**
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    */
  def reverse = ???

  /**
    * Implement append in terms of either foldLeft or foldRight.
    */

  def append: Unit = ???

  /**
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    */

  def map[A,B](as: List[A])(f: A => B): List[B] = ???

  def filter[A](as: List[A])(f: A => Boolean): List[A] = ???

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

}

val testList = List(1, 2, 3, 4)
val testList2 = List(1)
val testEmptyList = List()

// tail tests
assert(tail(testEmptyList) == Nil)
assert(tail(testList) == List(2, 3, 4))

// drop tests
assert(drop(testList, 2) == List(3, 4))
assert(drop(testList2, 2) == Nil)
assert(drop(testEmptyList, 2) == Nil)
assert(drop(testList2, 0) == testList2)
assert(drop(testList, 0) == testList)
