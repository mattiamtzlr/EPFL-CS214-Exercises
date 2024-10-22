package specs

object Translations:
  val translations = Map[String, Unit](
    "The list of integers l is sorted in ascending order" -> {
      val l = List(1, 2, 3, 4, 5)
      l.zip(l.tail).forall((a, b) => a <= b)
    },

    "All values in map m1 are keys in map m2" -> {
      val m1 = Map(true -> 'I', false -> 'O')
      val m2 = Map('I' -> 1, 'O' -> 0)
      m1.values.forall(m2.contains)
    },

    "Even numbers are always immediately followed by odd numbers in list l" -> {
      val l = List(1, 2, 3, 4, 5)
      (0 until l.length - 1).forall(i => l(i) % 2 == 1 || l(i + 1) % 2 == 1)
    },

    "Number p is prime" -> {
      val n = 17
      n > 1 && (2 to n - 1).forall(n % _ != 0)
    }
  )

case class Edge[+Point](src: Point, dst: Point)

def isPath[Point](edges: Seq[Edge[Point]]): Boolean =
  def consistent(edges: Seq[Edge[Point]]): Boolean = edges match
    case e0 :: e1 :: t => e0.dst == e1.src && consistent(e1 :: t)
    case _             => true
  
  !edges.isEmpty && consistent(edges)

def isCycle[Point](edges: Seq[Edge[Point]]): Boolean =
  isPath(edges) && edges.head.src == edges.last.dst