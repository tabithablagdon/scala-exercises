sealed trait Stream[+A] {
  def headOption: Option[A]
  def toList: List[A]
  def take(n :Int): Stream[A]
  def drop(n :Int): Stream[A]
  def takeWhile(p: A => Boolean): Stream[A]
  def forAll(p: A => Boolean): Boolean

  def foldRight[B](z: => B)(f: (A, => B) => B): B

  def takeWhileFoldR(p: A => Boolean): Stream[A]
  def headOptionFoldR: Option[A]
  def mapFoldR[B](f: A => B): Stream[B]
  def filterFoldR(f: A => Boolean): Stream[A]
  def appendFoldR[B >: A](b: => Stream[B]): Stream[B]
  def flatMapFoldR[B](f: A => Stream[B]): Stream[B]

  def mapUnfold[B](f: A => B): Stream[B] =
    Stream.unfold[B, Stream[A]](this){
      case Empty => None
      case Cons(h, tail) => Some((f(h()), tail()))
    }
  def takeUnfold(n :Int): Stream[A] =
    Stream.unfold[A, (Int, Stream[A])]((n, this)){
      case (n, Cons(h, tail)) if n > 0 => Some((h(), (n - 1, tail())))
      case _ => None
    }
  def takeWhileUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold[A, Stream[A]](this){
      case Cons(h, tail) if p(h()) => Some((h(), tail()))
      case _ => None
    }
  def zipWithUnfold[B](s2: Stream[B]): Stream[(A,B)] =
    Stream.unfold[(A,B), (Stream[A], Stream[B])]((this, s2)){
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some(((s1h(), s2h()), (s1t(),s2t())))
    }
  def zipAllUnfold[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold[(Option[A],Option[B]), (Stream[A], Stream[B])]((this, s2)){
      case (Empty, Empty) => None
      case (Empty, Cons(s2h, s2t)) => Some(((None, Some(s2h())), (Empty,s2t())))
      case (Cons(s1h, s1t), Empty) => Some(((Some(s1h()), None), (s1t(),Empty)))
      case (Cons(s1h, s1t), Cons(s2h, s2t)) => Some(((Some(s1h()), Some(s2h())), (s1t(),s2t())))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAllUnfold(s).forAll{
      case(a, None) => true
      case(a, b) => a == b
    }
  def tails: Stream[Stream[A]] = {
    val rest =
      Stream.unfold[Stream[A], Stream[A]](this){
        case Cons(_, tail) => Some((tail(), tail()))
        case _ => None
      }
    Cons(() => this, () => rest)
  }
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.mapUnfold(_.foldRight(z)(f))

}

case object Empty extends Stream[Nothing] {
  override def headOption: Option[Nothing] = None
  override def toList: List[Nothing] = List.empty[Nothing]
  override def take(n :Int): Stream[Nothing] = Empty
  override def drop(n :Int): Stream[Nothing] = Empty
  override def takeWhile(p: Nothing => Boolean): Stream[Nothing] = Empty
  override def forAll(p: Nothing => Boolean): Boolean = true

  override def foldRight[B](z: => B)(f: (Nothing, => B) => B): B = z

  override def takeWhileFoldR(p: Nothing => Boolean): Stream[Nothing] = Empty
  override def headOptionFoldR: Option[Nothing] = None
  override def mapFoldR[B](f: Nothing => B): Stream[B] = Empty
  override def filterFoldR(f: Nothing => Boolean): Stream[Nothing] = Empty
  override def appendFoldR[B >: Nothing](b: => Stream[B]): Stream[B] = b
  override def flatMapFoldR[B](f: Nothing => Stream[B]): Stream[B] = Empty
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
  override def headOption: Option[A] = Some(h())
  override def toList: List[A] = h() :: t().toList
  override def take(n :Int): Stream[A] =
    n match {
      case 0 => Empty
      case _ => Cons(h, () => t().take(n-1))
    }
  override def drop(n :Int): Stream[A] =
    n match {
      case 0 => this
      case _ => t().drop(n-1)
    }
  override def takeWhile(p: A => Boolean): Stream[A] =
    if(p(h())) Cons(h, () => t().takeWhile(p))
    else Empty
  override def forAll(p: A => Boolean): Boolean =
    if(p(h())) t().forAll(p)
    else false

  override def foldRight[B](z: => B)(f: (A, => B) => B): B =
    f(h(), t().foldRight(z)(f))

  override def takeWhileFoldR(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]){
      case (element, taken) if p(element) => Cons(() => element, () => taken)
      case _ => Stream.empty[A]
    }
  override def headOptionFoldR: Option[A] =
    foldRight(Option.empty[A]){ case (a, _) => Some(a) }
  override def mapFoldR[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((element, result) => Cons(() => f(element), () => result))
  override def filterFoldR(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) {
      case (element, taken) if f(element) => Cons(() => element, () => taken)
      case (_, taken) => taken
    }
  override def appendFoldR[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((element, result) => Cons(() => element, () => result))
  override def flatMapFoldR[B](f: A => Stream[B]): Stream[B] =
    mapFoldR(f).foldRight(Stream.empty[B])((element, result) => element.appendFoldR(result))
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))
  def fib(a: Int = 0, b: Int = 1): Stream[Int] = Stream.cons(a, fib(b, a+b))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }

  lazy val onesUnfold: Stream[Int] = unfold(())(_ => Some((1, ())))
  def constantUnfold[A](a: A): Stream[A] = unfold(())(_ => Some((a, ())))
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n+1)))
}

assert(Stream().headOption.isEmpty)
assert(Stream(1,2,3).headOption.contains(1))

assert(Stream().toList == List())
assert(Stream(1,2,3).toList == List(1,2,3))

assert(Stream(1,2,3).take(0).toList == Stream().toList)
assert(Stream(1,2,3).take(1).toList == Stream(1).toList)
assert(Stream(1,2,3).take(2).toList == Stream(1,2).toList)
assert(Stream(1,2,3).take(3).toList == Stream(1,2,3).toList)
assert(Stream(1,2,3).take(4).toList == Stream(1,2,3).toList)

assert(Stream(1,2,3).drop(0).toList == Stream(1,2,3).toList)
assert(Stream(1,2,3).drop(1).toList == Stream(2,3).toList)
assert(Stream(1,2,3).drop(2).toList == Stream(3).toList)
assert(Stream(1,2,3).drop(3).toList == Stream().toList)
assert(Stream(1,2,3).drop(4).toList == Stream().toList)

assert(Stream(1,2,3).takeWhile(a => a < 1).toList == Stream().toList)
assert(Stream(1,2,3).takeWhile(a => a < 2).toList == Stream(1).toList)
assert(Stream(1,2,3).takeWhile(a => a < 3).toList == Stream(1,2).toList)
assert(Stream(1,2,3).takeWhile(a => a < 4).toList == Stream(1,2,3).toList)

assert(Stream(1,2,3).forAll(a => a < 1) == false)
assert(Stream(1,2,3).forAll(a => a < 2) == false)
assert(Stream(1,2,3).forAll(a => a < 3) == false)
assert(Stream(1,2,3).forAll(a => a < 4) == true)

assert(Stream(1,2,3).takeWhile(a => a < 1).toList == Stream(1,2,3).takeWhileFoldR(a => a < 1).toList)
assert(Stream(1,2,3).takeWhile(a => a < 2).toList == Stream(1,2,3).takeWhileFoldR(a => a < 2).toList)
assert(Stream(1,2,3).takeWhile(a => a < 3).toList == Stream(1,2,3).takeWhileFoldR(a => a < 3).toList)
assert(Stream(1,2,3).takeWhile(a => a < 4).toList == Stream(1,2,3).takeWhileFoldR(a => a < 4).toList)

assert(Stream().headOptionFoldR.isEmpty)
assert(Stream(1,2,3).headOptionFoldR.contains(1))

assert(Stream(1,2,3).mapFoldR(_ * 2).toList == Stream(2,4,6).toList)

assert(Stream(1,2,3,4).filterFoldR(_ % 2 == 0).toList == Stream(2,4).toList)

assert(Stream(1,2,3).appendFoldR(Stream(4,5,6)).toList == Stream(1,2,3,4,5,6).toList)
assert(Stream(1,2,3).appendFoldR(Stream()).toList == Stream(1,2,3).toList)
assert(Stream().appendFoldR(Stream(4,5,6)).toList == Stream(4,5,6).toList)

assert(Stream(1,2,3,4).flatMapFoldR(a => Stream(a, a + 1)).toList == Stream(1,2,2,3,3,4,4,5).toList)

assert(Stream.constant(2).take(4).toList == Stream(2,2,2,2).toList)
assert(Stream.from(1).take(4).toList == Stream(1,2,3,4).toList)
assert(Stream.fib().take(7).toList == Stream(0,1,1,2,3,5,8).toList)

val countdown = Stream.unfold(10) { x => if (x == 0) None else Some(x, x - 1) }
assert(countdown.toList == Stream(10,9,8,7,6,5,4,3,2,1).toList)

assert(Stream.onesUnfold.take(4).toList == Stream(1,1,1,1).toList)
assert(Stream.constantUnfold(2).take(4).toList == Stream(2,2,2,2).toList)
assert(Stream.fromUnfold(1).take(4).toList == Stream(1,2,3,4).toList)

assert(Stream.empty[Int].mapUnfold(_ * 2).toList == Stream.empty[Int].mapFoldR(_ * 2).toList)
assert(Stream(1,2,3).mapUnfold(_ * 2).toList == Stream(1,2,3).mapFoldR(_ * 2).toList)

assert(Stream.empty[Int].take(5).toList == Stream.empty[Int].takeUnfold(5).toList)
assert(Stream(1,2,3).take(0).toList == Stream(1,2,3).takeUnfold(0).toList)
assert(Stream(1,2,3).take(1).toList == Stream(1,2,3).takeUnfold(1).toList)
assert(Stream(1,2,3).take(2).toList == Stream(1,2,3).takeUnfold(2).toList)
assert(Stream(1,2,3).take(3).toList == Stream(1,2,3).takeUnfold(3).toList)
assert(Stream(1,2,3).take(4).toList == Stream(1,2,3).takeUnfold(4).toList)

assert(Stream.empty[Int].takeWhile(a => a < 4).toList == Stream.empty[Int].takeWhileUnfold(a => a < 4).toList)
assert(Stream(1,2,3).takeWhile(a => a < 1).toList == Stream(1,2,3).takeWhileUnfold(a => a < 1).toList)
assert(Stream(1,2,3).takeWhile(a => a < 2).toList == Stream(1,2,3).takeWhileUnfold(a => a < 2).toList)
assert(Stream(1,2,3).takeWhile(a => a < 3).toList == Stream(1,2,3).takeWhileUnfold(a => a < 3).toList)
assert(Stream(1,2,3).takeWhile(a => a < 4).toList == Stream(1,2,3).takeWhileUnfold(a => a < 4).toList)

assert(Stream.empty[Int].zipWithUnfold(Stream.from(1)).toList == Stream.empty[Int].toList)
assert(Stream(1,2,3).zipWithUnfold(Stream(4,5,6,7,8,9)).toList == Stream((1,4),(2,5),(3,6)).toList)
assert(Stream(1,2,3,4,5,6).zipWithUnfold(Stream(4,5,6)).toList == Stream((1,4),(2,5),(3,6)).toList)

assert(Stream(1,2,3).zipAllUnfold(Stream(4,5,6,7)).toList == List((Some(1),Some(4)), (Some(2),Some(5)), (Some(3),Some(6)), (None,Some(7))))
assert(Stream(1,2,3,4).zipAllUnfold(Stream(4,5,6)).toList == List((Some(1),Some(4)), (Some(2),Some(5)), (Some(3),Some(6)), (Some(4),None)))

assert(Stream(1,2,3).startsWith(Stream(1)))
assert(Stream(1,2,3).startsWith(Stream(1,2)))
assert(Stream(1,2,3).startsWith(Stream(1,2,3)))
assert(!Stream(1,2,3).startsWith(Stream(1,2,3,4)))
assert(!Stream(1,2,3).startsWith(Stream(4,1,2)))
assert(!Stream(1,2,3).startsWith(Stream(4)))
assert(!Stream(1,2,3).startsWith(Stream(2)))

assert(Stream(1,2,3).tails.mapFoldR(_.toList).toList == List(List(1,2,3), List(2,3), List(3), List()))

assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6,5,3,0))