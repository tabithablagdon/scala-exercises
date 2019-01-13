import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}

def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)


// EXERCISE 3.7
// Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list. This is a deeper question that we’ll return to in chapter 5.


// ???


// ** EXERCISE 3.8
// See what happens when you pass Nil and Cons themselves to foldRight, like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).10 What do you think this says about the relationship between foldRight and the data constructors of List?

foldRight(Cons(1,Cons(2,Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
// A: The data constructor of List is implemented in terms of fold right?
object List{
  def apply[T](xs :T*): List[T] =
    xs.foldRight(Nil:List[T])(Cons(_,_))
}


// ** EXERCISE 3.9
// Compute the length of a list using foldRight.
def length[A](as: List[A]): Int =
  foldRight(as, 0)( (a, b) => 1 + b)

length(List(1, 2, 3, 4, 5, 6))


// ** EXERCISE 3.10
@tailrec
def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case Nil => z
  case Cons(x, xs) => foldLeft(xs,f(z, x))(f)
}

foldRight(List("b", "c", "d"),"a")(_ + _)
foldLeft(List("b", "c", "d"),"a")(_ + _)


// ** EXERCISE 3.11
// associative and comm (so no different)
def sum3(ns: List[Int]) = foldLeft(ns, 0)((x,y) => x + y)
def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

sum2(List(1, 2, 3, 4, 5))
sum3(List(1, 2, 3, 4, 5))
product2(List(1, 2, 3, 4, 5))
product3(List(1, 2, 3, 4, 5))

def length3[A](as: List[A]): Int =
  foldLeft(as, 0)( (b, _) => b + 1)

length3(List(1, 2, 3, 4, 5, 6))


// ** EXERCISE 3.12
def reverse[T](xs: List[T]): List[T] =
  foldLeft(xs, Nil:List[T])( (b, a) => Cons(a, b) )


reverse(List(1, 2, 3, 4, 5, 6))


// ** EXERCISE 3.13
def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  foldLeft(reverse(as), z)( (b,a) => f(a,b) )

foldRight(List("b", "c", "d"),"a")(_ + _)
foldRight2(List("b", "c", "d"),"a")(_ + _)

// ** EXERCISE 3.14
// Implement append in terms of either foldLeft or foldRight.
def append[T](xs: List[T], x: T): List[T] =
  foldRight(xs, Cons(x, Nil:List[T] ))( Cons(_, _ ) )

append(List(1, 2, 3, 4), 5)

// ** EXERCISE 3.15
// Hard: Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists. Try to use functions we have already defined
def merge[T](xs: List[T], ys: List[T]): List[T] =
  foldRight(xs, ys)( Cons(_, _ ) )

def concat[T](xs: List[List[T]]): List[T]  = xs match{
  case Nil => Nil
  case Cons(x, xs) => merge(x, concat(xs))
}


// ** EXERCISE 3.16
def transformBy1(xs: List[Int]): List[Int] = xs match{
  case Nil => Nil
  case Cons(x, xs) => Cons(x+1, transformBy1(xs))
}

transformBy1(List(1, 2, 3, 4))

// ** EXERCISE 3.17
def doubleListToString(xs: List[Double]): List[String] = xs match{
  case Nil => Nil
  case Cons(x, xs) => Cons(x.toString, doubleListToString(xs))
}

doubleListToString(List(1.0, 2.0, 3.0, 4.0))


// ** EXERCISE 3.18
def map[A,B](as: List[A])(f: A => B): List[B] = as match{
  case Nil => Nil
  case Cons(x, xs) => Cons(f(x), map(xs)(f))
}

map(List(1, 2, 3, 4)){x => x+1}

// ** EXERCISE 3.19
def filter[A](as: List[A])(f: A => Boolean): List[A] = as match{
  case Nil => Nil
  case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
  case Cons(_, xs) => filter(xs)(f)
}


filter(List(1, 2, 3, 4, 5, 6)){ x => x%2 == 0}


// ** EXERCISE 3.20
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  concat(map(as)(f))

flatMap(List(1,2,3))(i => List(i,i))


// ** EXERCISE 3.21
def filter2[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap(as){ x => if(f(x)) List(x) else Nil }

filter2(List(1, 2, 3, 4, 5, 6)){ x => x%2 == 0}

// ** EXERCISE 3.22
def zipAdd(xs: List[Int], ys:List[Int]): List[Int] = (xs, ys) match{
  case (Nil, Nil) => Nil
  case (Cons(x, xs), Nil) => Cons(x, zipAdd(xs, Nil))
  case (Nil, Cons(x, xs)) => Cons(x, zipAdd(xs, Nil))
  case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, zipAdd(xs, ys))
}

zipAdd(List(1,2,3), List(4,5,6))

// ** EXERCISE 3.23
def zipWith[T](xs: List[T], ys: List[T])( f: (T, T) => T ): List[T] = (xs, ys) match{
  case (Nil, Nil) => Nil
  case (Cons(x, xs), Nil) => Cons(x, zipWith(xs, Nil)(f))
  case (Nil, Cons(x, ys)) => Cons(x, zipWith(Nil, ys)(f))
  case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
}

zipWith(List(1,2,3), List(4,5,6))(_ + _)