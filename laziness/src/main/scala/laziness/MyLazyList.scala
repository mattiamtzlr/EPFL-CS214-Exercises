package laziness

class MyLazyList[+A](init: () => MyLazyListState[A]):
  lazy val state: MyLazyListState[A] = init()

enum MyLazyListState[+A]:
  case LZCons(elem: A, tail: MyLazyList[A])
  case LZNil

object MyLazyList:

  import MyLazyListState.*

  def cons[A](elem: => A, tail: => MyLazyList[A]): MyLazyList[A] =
    MyLazyList(() => LZCons(elem, tail))

  def empty: MyLazyList[Nothing] = MyLazyList(() => LZNil)

  extension [A](self: MyLazyList[A])

    def isEmpty: Boolean = self.state match
      case LZNil        => true
      case LZCons(_, _) => false

    def head: A = self.state match
      case LZNil        => throw RuntimeException("head of empty list")
      case LZCons(x, _) => x

    def tail: MyLazyList[A] = self.state match
      case LZNil         => throw RuntimeException("tail of empty list")
      case LZCons(_, xs) => xs

    def size: Int = self.state match
      case LZNil         => 0
      case LZCons(_, xs) => 1 + xs.size

    def foreach(f: A => Unit): Unit = self.state match
      case LZNil => ()
      case LZCons(x, xs) =>
        f(x)
        xs.foreach(f)

    def contains(elem: A): Boolean = self.state match
      case LZNil => false
      case LZCons(x, xs) =>
        if x == elem then true
        else xs.contains(elem)

    def get(i: Int): A =
      if i < 0 then throw RuntimeException("index out of bounds")
      else
        self.state match
          case LZNil => throw RuntimeException("index out of bounds")
          case LZCons(x, xs) =>
            if i == 0 then x
            else xs.get(i - 1)

    def wrongTake(n: Int): MyLazyList[A] =
      if n <= 0 then empty
      else
        self.state match
          case LZNil         => empty
          case LZCons(x, xs) => cons(x, xs.wrongTake(n - 1))

    def take(n: Int): MyLazyList[A] =
      if n <= 0 then empty
      else MyLazyList(() =>
        self.state match
          case LZNil         => LZNil
          case LZCons(x, xs) => LZCons(x, xs.take(n - 1))
      )

    def drop(n: Int): MyLazyList[A] =
      if n <= 0 then self
      else MyLazyList(() =>
        self.state match
          case LZNil         => LZNil
          case LZCons(_, xs) => xs.drop(n - 1).state
        
      )

    def map[B](f: A => B): MyLazyList[B] =
      MyLazyList(() =>
        self.state match
          case LZNil         => LZNil
          case LZCons(x, xs) => LZCons(f(x), xs.map(f))
      )

    def filter(p: A => Boolean): MyLazyList[A] =
      MyLazyList(() =>
        self.state match
          case LZNil         => LZNil
          case LZCons(x, xs) => 
            if p(x) then LZCons(x, xs.filter(p)) 
            else xs.filter(p).state
      )

    def zip[B](that: MyLazyList[B]): MyLazyList[(A, B)] =
      MyLazyList(() =>
        (self.state, that.state) match
          case (LZNil, LZNil)                 => LZNil
          case (LZNil, _) | (_, LZNil)        => LZNil
          case (LZCons(x, xs), LZCons(y, ys)) => LZCons((x, y), xs.zip(ys))
      )

    def append(that: MyLazyList[A]): MyLazyList[A] =
      MyLazyList(() => 
        self.state match
          case LZNil         => that.state
          case LZCons(x, xs) => LZCons(x, xs.append(that))
      )

    def flatMap[B](f: A => MyLazyList[B]): MyLazyList[B] =
      MyLazyList(() =>
        self.state match
          case LZNil         => LZNil
          case LZCons(x, xs) => f(x).append(xs.flatMap(f)).state
      )
