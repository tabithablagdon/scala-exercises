
trait RNG {
  def nextInt: (Int, RNG)
}

type Rand[+A] = RNG => (A, RNG)

def unit[A](a: A): Rand[A] = rng => (a, rng)
def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

// 6.6
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, rng2) = ra(rng)
  val (b, rng3) = rb(rng2)
  (f(a,b), rng3)
}
def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

object RNG{
  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    (if(v == Int.MinValue) 0 else if( v < 0) -v else v, rng2)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (v, r) = nonNegativeInt(rng)
    (v.toDouble/Int.MaxValue, r)
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = RNG.double(r)
    ((i, d), r2)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val x = RNG.intDouble(rng)
    ((x._1._2, x._1._1),x._2)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d, r) = RNG.double(rng)
    val (d2, r2) = RNG.double(r)
    val (d3, r3) = RNG.double(r2)
    ((d, d2, d3), r3)
  }
  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (0 to count).foldLeft( (List.empty[Int], rng) ){
      case ((xs, r), _) =>
        val (i, r2) = r.nextInt
        (i :: xs, r2)
    }
  }


}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}


// 6.1
RNG.nonNegativeInt(SimpleRNG(new scala.util.Random().nextInt()))

// 6.2
RNG.double(SimpleRNG(new scala.util.Random().nextInt()))

// 6.3
RNG.intDouble(SimpleRNG(new scala.util.Random().nextInt()))
RNG.doubleInt(SimpleRNG(new scala.util.Random().nextInt()))
RNG.double3(SimpleRNG(new scala.util.Random().nextInt()))

// 6.4
RNG.ints(8)(SimpleRNG(42))

val int: Rand[Int] = _.nextInt


def nonNegativeEven: Rand[Int] = map(RNG.nonNegativeInt)(i => i - i % 2)
nonNegativeEven(SimpleRNG(new scala.util.Random().nextInt()))

// 6.5
def double: Rand[Double] = map(RNG.nonNegativeInt)(_.toDouble)
double(SimpleRNG(new scala.util.Random().nextInt()))

val randIntDouble: Rand[(Int, Double)] = both(int, double)
val randDoubleInt: Rand[(Double, Int)] = both(double, int)


// 6.6
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
  fs.foldRight( (List.empty[A],rng) ){ case (r, (xs, b)) =>
    val (x, r2) = r(b)
    (x :: xs, r2)
  }
}

def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  sequence(List.fill(count){ r:RNG =>
    r.nextInt
  })(rng)



ints(10)( SimpleRNG(new scala.util.Random().nextInt()) )

// 6.8
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
  val (a, r) = f(rng)
  g(a)(r)
}


def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(RNG.nonNegativeInt){ i =>
    val mod = i % n
    if (i + (n - 1) - mod >= 0)
      (r:RNG) => { (mod, r) }
    else
      nonNegativeLessThan(n)
  }


nonNegativeLessThan(24)( SimpleRNG(new scala.util.Random().nextInt()) )


// 6.9
def mapB[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s){ a => (r: RNG) => {(f(a), r)} }

mapB[Int, Int]( RNG.nonNegativeInt ){ _ * 2 }( SimpleRNG(new scala.util.Random().nextInt()) )

def map2B[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
  flatMap( ra ){ a =>
    flatMap(rb){ b =>
      (r: RNG) => { (f(a,b), r) }
    }
  }

map2B( RNG.nonNegativeInt, RNG.double ){ (x,y) => x+y }( SimpleRNG(new scala.util.Random().nextInt()) )


// 6.10
case class State[S,+A](run: S => (A,S)){
  def map[B](f: A => B): State[S, B] =
    State[S, B]( { s:S => val (a, s2) = this.run(s); (f(a), s2) } )

  def flatMap[B](g: A => State[S, B]): State[S, B] = State({ s:S =>
    val (a, s2) = this.run(s)
    g(a).run(s2)
  })
  def map2[B,C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap { a: A =>
      rb.flatMap { b: B =>
        State( (s: S) => { (f(a, b), s) } )
      }
    }
  def sequence[B >: A](xs: List[State[S, B]]): State[S, List[B]] = State({ st: S =>
    xs.foldRight( (List.empty[B], st) ){ case (ns, (xs, s)) =>
      val (b, s2) = ns.run(s)
      (b :: xs, s2)
    }
  })

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
object State{
  def unit[S, A](a: A): State[S, A] = State( {s: S => (a, s) } )
}

type Rand2[A] = State[RNG, A]

val int2: Rand2[Int] = State({r: RNG => r.nextInt })
def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
  sequence(List.fill(count){ r:RNG =>
    r.nextInt
  })(rng)

val rand2 = State.unit( 0 )

val ns  = int2.flatMap(x =>
  int2.map(y =>
    x + y))


ns.run( SimpleRNG(new scala.util.Random().nextInt()) )


// 6.11
sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
  State( {m: Machine => inputs.foldRight( ((m.coins,m.candies),m) ){ case b @ (input, ((coins: Int, candies: Int), machine: Machine)) =>
    input match{
      case Coin if machine.locked && machine.candies > 0 => ((coins,candies), machine.copy(locked = false))
      case Turn if !machine.locked && machine.candies > 0 => ((coins+1,candies-1), machine.copy(locked = true, candies = candies-1))
      case _ => b._2
    }
  }})


simulateMachine( List[Input](Turn, Coin, Turn, Coin, Turn, Coin, Turn, Coin) ).run( Machine(true, 5, 10) )
simulateMachine( List[Input](Turn, Coin, Turn, Turn, Turn, Turn, Coin, Turn, Coin, Turn, Coin) ).run( Machine(true, 5, 10) )




