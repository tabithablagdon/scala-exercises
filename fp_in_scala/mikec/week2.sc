sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

/**
  * Generalize tail to the function drop, which removes the first n elements from a list.
  * Note that this function takes time proportional only to the number of elements being dropped—
  * we don't need to make a copy of the entire List.
  */

@scala.annotation.tailrec
def drop[A](l: List[A], n: Int): List[A] =
  (l, n) match {
    case (Nil, _) => Nil
    case (remaining, 0) => remaining
    case (Cons(_, remaining), n) => drop(remaining, n - 1)
  }

val l123: List[Int] = List(1,2,3)

assert(drop(Nil, 0) == Nil)
assert(drop(Nil, 100) == Nil)
assert(drop(l123, 0) == l123)
assert(drop(l123, 1) == List(2,3))
assert(drop(l123, 2) == List(3))
assert(drop(l123, 3) == Nil)
assert(drop(l123, 4) == Nil)

/** Implement dropWhile, which removes elements from the List prefix as long as they match a predicate. */

@scala.annotation.tailrec
def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match {
    case Nil => Nil
    case remaining@Cons(head, _) if !f(head) => remaining
    case Cons(_, remaining) => dropWhile(remaining, f)
  }

assert(dropWhile[Int](Nil, _ => true) == Nil)
assert(dropWhile[Int](Nil, _ => false) == Nil)
assert(dropWhile[Int](l123, a => a < 4) == Nil)
assert(dropWhile[Int](l123, a => a >= 4) == l123)
assert(dropWhile[Int](l123, a => a <= 2) == Cons(3,Nil))

/**
  * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but
  * the last element of a List. So, given List(1,2,3,4), init will return List(1,2,3). Why can't
  * this function be implemented in constant time like tail?
  */

def init[A](l: List[A]): List[A] = {
  l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }
}

assert(init[Int](Nil) == Nil)
assert(init[Int](Cons(1, Nil)) == Nil)
assert(init[Int](Cons(1, Cons(2, Nil))) == Cons(1, Nil))
assert(init[Int](Cons(1, Cons(2, Cons(3, Nil)))) == Cons(1, Cons(2, Nil)))

// Solution is linear because this is linked list, reaching last element requires traversal of whole list

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

/**
  * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it
  * encounters a 0.0? Why or why not? Consider how any short-circuiting might work if you call
  * foldRight with a large list. This is a deeper question that we'll return to in chapter 5.
  */

// No, f fully realizes params A and B, so foldRight will evaluate the entire list before first applying f.
// Over a large list, you get a stack overflow b/c foldRight is not tail recursive.

/**
  * See what happens when you pass Nil and Cons themselves to foldRight, like this:
  * foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this says about the
  * relationship between foldRight and the data constructors of List?
  */

val reconstructed = foldRight[Int, List[Int]](List(1,2,3), Nil)(Cons(_,_))

// It reconstructs the list. The constructor can be implemented with foldRight.

/** Compute the length of a list using foldRight. */

def length[A](as: List[A]): Int =
  foldRight(as, 0)((_, sum) => sum + 1)

assert(length(Nil) == 0)
assert(length(List(1)) == 1)
assert(length(List(1,2)) == 2)
assert(length(List(1,2,3)) == 3)
assert(length(List(1,2,3,4)) == 4)

/**
  * Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError for large lists
  * (we say it's not stack-safe). Convince yourself that this is the case, and then write another general
  * list-recursion function, foldLeft, that is tail-recursive, using the techniques we discussed in the previous
  * chapter. Here is its signature:11
  */

@scala.annotation.tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

/** Write sum, product, and a function to compute the length of a list using foldLeft. */

def sum(as: List[Int]): Int =
  foldLeft(as, 0)(_ + _)

assert(sum(Nil) == 0)
assert(sum(List(1)) == 1)
assert(sum(List(1,2)) == 3)
assert(sum(List(1,2,3)) == 6)
assert(sum(List(1,2,3,4)) == 10)

def product(as: List[Int]): Int =
  foldLeft(as, 1)(_ * _)

assert(product(Nil) == 1)
assert(product(List(1)) == 1)
assert(product(List(1,2)) == 2)
assert(product(List(1,2,3)) == 6)
assert(product(List(1,2,3,4)) == 24)

def length2(as: List[Int]): Int =
  foldLeft(as, 0)((sum, _) => sum  + 1)

assert(length2(Nil) == 0)
assert(length2(List(1)) == 1)
assert(length2(List(1,2)) == 2)
assert(length2(List(1,2,3)) == 3)
assert(length2(List(1,2,3,4)) == 4)

/**
  * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
  * See if you can write it using a fold.
  */
def reverse[A](as: List[A]): List[A] =
  foldLeft(as, List[A]())((b,a) => Cons[A](a,b))

assert(reverse(List[Int]()) == Nil)
assert(reverse(List(1)) == List(1))
assert(reverse(List(1,2)) == List(2,1))
assert(reverse(List(1,2,3)) == List(3,2,1))
assert(reverse(List(1,2,3,4)) == List(4,3,2,1))

/** Implement append in terms of either foldLeft or foldRight. */

def append[A](as: List[A], bs: List[A]): List[A] =
  foldRight(as, bs)(Cons(_,_))

assert(append(List(1,2,3,4), List(5,6,7,8)) == List(1,2,3,4,5,6,7,8))

/**
  * Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in
  * the total length of all lists. Try to use functions we have already defined.
  */

def map[A,B](as: List[A])(f: A => B): List[B] =
  foldRight[A,List[B]](as, List[B]())((element, result) => Cons(f(element), result))

assert(map(List(1,2,3,4))((a:Int) => a * 2) == List(2,4,6,8))

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  foldRight(map(as)(f), List[B]())(append)

assert(flatMap(List(1,2,3,4))((a:Int) => List(a, a + 1)) == List(1,2,2,3,3,4,4,5))

def filter[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap[A,A](as)(a => if(f(a)) List(a) else List())

assert(filter(List(1,2,3,4))(a => a % 2 == 0) == List(2,4))