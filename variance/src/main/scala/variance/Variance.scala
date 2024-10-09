package variance

trait Stack[+T] extends HasTop[T]:
  /** Peek at the top of this stack */
  def peek(): Option[T]

  /** Create a new stack with one more entry, at the top */
  def push[S >: T](s: S): Stack[S]

  /** Separate the top entry from the rest of the stack */
  def pop(): (Option[T], Stack[T])

  def top: Option[T] = peek()

/**
  * Stack implementation using a List to hold the elements
  */
case class ListStack[T](l: List[T]) extends Stack[T]:
  def peek(): Option[T] = l.headOption
  def push[S >: T](s: S): Stack[S] = ListStack(s :: l)
  def pop(): (Option[T], Stack[T]) = l match
    case Nil    => (None, ListStack(Nil))
    case h :: t => (Option(h), ListStack(t))

def joinStacks[T](l: List[Stack[T]]): Stack[T] =
  ListStack(l.flatMap(_.peek()))

def mkStackInt(): Stack[Int] = ListStack(List(1, 2, 3))

def mkStackString(): Stack[String] = ListStack(List("a", "b", "c"))

// Does this work?
// val tops = joinStacks(List(mkStackInt(), mkStackString())) - nope

trait Drawer[T]:
  def get(): T
  def put(t: T): Drawer[T]

case class IncrementingDrawer(i: Int) extends Drawer[Int]:
  def get() = i - 1
  def put(j: Int) = IncrementingDrawer(j + 1)


trait Box[+T] extends HasTop[T]:
  /** Peek at the value inside the box */
  def unbox(): Option[T]

  /** Create a new box with the contents */
  def replace[S >: T](s: S): Box[S]

  /** Create a new box by applying `f` to the contents of this one */
  def map[T2](f: T => T2): Box[T2]

  def top: Option[T] = unbox()

trait HasTop[+T]:
  /** Peek at one value inside this container */
  def top: Option[T]


def joinTops[T](l: List[HasTop[T]]): List[T] =
  l.flatMap(_.top)