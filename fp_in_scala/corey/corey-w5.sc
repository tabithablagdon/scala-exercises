import scala.annotation.tailrec

sealed trait Stream[+A]{

  // 5.1
  def toList: List[A] = this match{
    case Empty => Nil
    case Cons(h, tl) => h() :: tl().toList
  }

  // 5.2
  def take(n: Int): Stream[A] = this match{
    case Cons(h, tl) if n > 0 => Cons(h, () => tl().take(n-1) )
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match{
    case Cons(h, tl) if n > 0 => tl().drop(n-1)
    case xs => xs
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match{
    case Cons(h, tl) if p(h())=> Cons(h, () => tl().takeWhile(p) )
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight( Stream.empty[A] ){ (a, b) =>
      if( p( a ) ) Stream.cons(a, b)
      else b
    }

  // 5.6
  // ?????
  def headOption: Option[A] =
    foldRight( Option.empty[A] ){ (a, b) =>
      if(a == Empty) None
      else Some(a)
    }

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight( Stream.empty[B] ){ (a,b) =>
      Stream.cons( f(a), b)
    }
  def filter(p: A => Boolean): Stream[A] =
    foldRight( Stream.empty[A] ){ (a,b) =>
      if( p(a) ) Stream.cons(a, b)
      else b
    }

  // https://stackoverflow.com/questions/43180310/covariant-type-a-occurs-in-contravariant-position-in-type-a-of-value-a
  def preAppend[B >: A](x: B): Stream[B] =
    Stream.cons(x, this)

  def append[B >: A](x: => B): Stream[B] =
    foldRight( Stream.cons(x, Stream.empty[B]) ){ (a, b) =>
      Stream.cons(a, b)
    }
  def merge[B >: A](x: Stream[B]): Stream[B] =
    foldRight( x ){ (a, b) =>
      Stream.cons(a, b)
    }
  def flatMap[B]( f: A => Stream[B]): Stream[B] =
    foldRight( Stream.empty[B] ){ (a, b) =>
      f(a).merge(b)
    }
  def find(p: A => Boolean): Option[A] = filter(p).headOption


  def map2[B](f: A => B): Stream[B] =
    Stream.unfold(this){
      case Cons(x, xs) => Some( (f(x()), xs()) )
      case Empty => None
    }
  def take2(n: Int): Stream[A] =
    Stream.unfold( (this, n) ){
      case (Cons(x, xs), n) if n > 0 => Some( (x()), (xs(), n-1) )
      case _ => None
    }
  def takeWhile3(p: A => Boolean): Stream[A] =
    Stream.unfold( this ){
      case Cons(x, xs) if p(x()) => Some( (x()), xs() )
      case _ => None
    }


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold( (this, s2) ){
      case (Cons(x, xs), Cons(y, ys)) => Some( (Some(x()),Some(y())), (xs(), ys()) )
      case (Empty, Cons(y, ys)) => Some( (None,Some(y())), (Empty, ys()) )
      case (Cons(x, xs), Empty) => Some( (Some(x()),None), (xs(), Empty) )
      case _ => None
    }


  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).find{
      case (Some(x), Some(y)) => x != y
      case (None, Some(_)) => true
      case _ => false
    }.isEmpty

  // 5.14
  def tails: Stream[Stream[A]] =
    Stream.unfold(this){
      case a @ Cons(x, xs) => Some( (a, xs()) )
      case _ => None
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  // 5.15
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    foldRight( (Stream.empty[B], z) ){ case (a, (xs,b)) =>
      (Stream.cons(f(a, b), xs), f(a, b))
    }._1
}
case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream{
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, Stream.constant(a))

  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n+1))

  def fibs(n: Int, m: Int): Stream[Int] =
    Stream.cons(n, fibs(m, n+m))


  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match{
      case Some( (a, s) ) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty[A]
    }


  def constant2[A](a: A): Stream[A] =
    Stream.unfold(a){ x => Some(x, x)}

  def from2(n: Int): Stream[Int] =
    Stream.unfold(n){ x => Some(x, x+1)}

  def fibs2(n: Int, m: Int): Stream[Int] =
    Stream.unfold( (n, m) ){ case (x1, x2) => Some(x1, (x2, x1+x2) )}
}

// 5.1
val s = Stream(1, 2, 3, 4, 5)
s.toList

// 5.2
val s2 = Stream(1, 2, 3, 4, 5)
s2.take(3).toList
val s3 = Stream(1, 2, 3, 4, 5)
s3.drop(3).toList

// 5.3
val s4 = Stream(1, 2, 3, 4, 5)
s4.takeWhile( {x => x < 4}).toList

// 5.4
val s5 = Stream(1, 2, 3, 4, 5)
s5.forAll( {x => x < 4})
s5.forAll( {x => x < 10})

// 5.5
val s6 = Stream(1, 2, 3, 4, 5)
s6.takeWhile2( {x => x < 4}).toList

// 5.6 - ???
val s7 = Stream(1, 2, 3, 4, 5)
s7.headOption
val s8 = Stream.empty[Int]
s8.headOption

// 5.7
val s9 = Stream(1, 2, 3, 4, 5)
s9.map( _ * 2).toList
s9.filter{x: Int => x % 2 == 0}.map( _ * 2).toList
s9.preAppend(6).toList
s9.append(6).toList
s9.flatMap{ x => Stream(x+1,x+2)}.toList

Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList

// 5.8
Stream.constant(6).take(3).toList

// 5.9
Stream.from(2).take(10).toList

// 5.10
Stream.fibs(0, 1).take(10).toList

// 5.11
Stream.unfold(1){x: Int => Some( (x+1, x*10) )}.take(10).toList

// 5.12
Stream.constant2(6).take(3).toList
Stream.from2(2).take(10).toList
Stream.fibs2(0, 1).take(10).toList

// 5.13
val s10 = Stream(1, 2, 3, 4, 5)
s10.map2( _ * 2).toList
s10.map2( _ * 2).take2(3).toList
s10.map2( _ * 2).takeWhile3{_ < 6}.toList
val s11 = Stream("a", "b", "c")
s10.zipAll(s11).toList

// 5.14
Stream(1,2,3) startsWith Stream(1,2)

// 5.15
val s12 = Stream(1, 2, 3)
s12.tails.map(_.toList).toList

val s13 = Stream(1, 2, 3, 4, 5, 6, 7, 8)
val s14 = Stream(4, 5, 6)
s13.hasSubsequence(s14)

// 5.16
Stream(1, 2, 3).scanRight(0)(_ + _).toList
