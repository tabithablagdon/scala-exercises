import scala.annotation.tailrec

/*
Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
 */

@tailrec
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
  as match{
    case xs if xs.length > 2 => ordered(xs(0), xs(1)) && isSorted(xs.drop(2), ordered)
    case _ => true
  }

val acc = Array(1, 2, 3, 4, 5, 6)
println( isSorted( acc, (a: Int, b: Int) => a < b ))
println( isSorted( acc, (a: Int, b: Int) => a > b ))

val accStr = Array('a', 'b', 'c', 'd', 'e')
println( isSorted( accStr, (a: Char, b: Char) => a < b ))
println( isSorted( accStr, (a: Char, b: Char) => a > b ))


/*
Let's look at another example, currying,9 which converts a function f of two arguments into a function of one argument that partially applies f.
Here again there's only one implementation that compiles. Write this implementation.
def curry[A,B,C](f: (A, B) => C): A => (B => C)
 */


def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (x: A) => f(x, _)


val min = (a: Int, b: Int) => Math.min(a,b)

println( curry(min)(10)(20) )
println( min(10, 20) )

println( curry(min)(20)(10) )
println( min(20, 10) )


/*
Implement uncurry, which reverses the transformation of curry. Note that since => associates to the right, A => (B => C)
can be written as A => B => C.
def uncurry[A,B,C](f: A => B => C): (A, B) => C
 */

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (x: A, y: B) => f(x)(y)

println( uncurry(curry(min))(10, 20) )
println( uncurry(curry(min))(20, 10) )


/*
Implement the higher-order function that composes two functions.
def compose[A,B,C](f: B => C, g: A => B): A => C
 */

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (x: A) => f(g(x))
