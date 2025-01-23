package ds

class DequeTest extends munit.FunSuite:
  test("deque: empty isEmpty"):
    assertEquals(Deque.apply[Int]().isEmpty, true)

  test("deque: empty popFirst"):
    assertEquals(Deque.apply[Int]().popFirst, None)

  test("deque: empty popLast"):
    assertEquals(Deque.apply[Int]().popLast, None)

  test("deque: push then pop"):
    var deque: Deque[Int] = DequeImpl[Int](Nil, Nil)
    deque = deque.pushFirst(42)
    assertEquals(deque.popFirst.get._1, 42)
    assertEquals(deque.popLast.get._1, 42)
    deque = deque.pushFirst(43)
    assertEquals(deque.popFirst.get._1, 43)
    assertEquals(deque.popLast.get._1, 42)
    deque = deque.pushLast(0)
    assertEquals(deque.popFirst.get._1, 43)
    assertEquals(deque.popLast.get._1, 0)
    assertEquals(deque.popLastAll, List(0, 42, 43))
    assertEquals(deque.popFirstAll, List(43, 42, 0))

  test("deque: simple test 1"):
    var deque = Deque[Int]()
    deque = deque.pushFirst(1)
    assertEquals(deque.isEmpty, false)
    assertEquals(deque.popFirst.map(_._1), Some(1))

  test("deque: simple test 2"):
    var deque = Deque[Int]()
    deque = deque.pushLast(1).pushLast(2).pushLast(3)
    assertEquals(deque.popFirst.map(_._1), Some(1))
    deque = deque.popFirst.map(_._2).get
    assertEquals(deque.popFirst.map(_._1), Some(2))
    deque = deque.popFirst.map(_._2).get
    assertEquals(deque.popFirst.map(_._1), Some(3))
    deque = deque.popFirst.map(_._2).get
    assertEquals(deque.popFirst, None)

  test("deque: simple test 3"):
    var deque = Deque[String]()
    deque = deque.pushFirst("hello").pushLast("world")
    assertEquals(deque.popFirst.map(_._1), Some("hello"))
    assertEquals(deque.popLast.map(_._1), Some("world"))

  test("deque: complex test 1"):
    var deque = Deque[Int]()
    for i <- 1 to 5 do
      deque = deque.pushFirst(i)
    for i <- 6 to 10 do
      deque = deque.pushLast(i)
    assertEquals(deque.popFirstAll, List(5, 4, 3, 2, 1, 6, 7, 8, 9, 10))

  test("deque: complex test 2"):
    var deque = Deque[Int]()
    val numbers = List(1, 2, 3, 4, 5)
    numbers.foreach(n => deque = if n % 2 == 0 then deque.pushFirst(n) else deque.pushLast(n))
    assertEquals(deque.popFirstAll, List(4, 2, 1, 3, 5))

  test("deque: complex test 3"):
    var deque = Deque[Int]()
    deque = deque.pushFirst(1).pushLast(2).pushFirst(3).pushLast(4)
    var sum = 0
    while !deque.isEmpty do
      deque.popFirst match
        case Some(x, d) =>
          sum += x
          deque = d
        case None => fail("Deque was empty")
    assertEquals(sum, 10)

  test("deque: complex test 4"):
    var deque1 = Deque[Int]()
    var deque2 = Deque[Int]()
    for i <- 1 to 5 do
      deque1 = deque1.pushFirst(i)
      deque2 = deque2.pushLast(i)
    assertEquals(deque1.popFirstAll.reverse, deque2.popFirstAll)
