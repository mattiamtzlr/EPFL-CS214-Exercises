package ds

class StackTest extends munit.FunSuite:
  test("stack: new stack always empty"):
    val s = Stack()
    assertEquals(s.isEmpty, true)

  test("stack: pop empty stack"):
    val s = Stack()
    assertEquals(s.pop, None)

  test("stack: push 42"):
    val s = StackImpl(List(1, 2, 3))
    assertEquals(s.push(42), StackImpl(List(42, 1, 2, 3)))

  test("stack: pop 42"):
    val s = StackImpl(List(42, 1, 2, 3))
    assertEquals(s.pop, Some(42, StackImpl(List(1, 2, 3))))

  test("stack: push, pop, push"):
    var stack: Stack[Int] = StackImpl[Int](Nil)
    stack = stack.push(1)
    stack = stack.push(2)
    stack.pop match
      case Some(x, stack1) =>
        stack = stack1
        assertEquals(x, 2)
      case None =>
    stack = stack.push(3)
    assertEquals(stack, StackImpl(List(3, 1)))

  test("stack: simple test 1"):
    var stack = Stack[Int]()
    stack = stack.push(42)
    assertEquals(stack.isEmpty, false)
    assertEquals(stack.pop.map(_._1), Some(42))

  test("stack: simple test 2"):
    var stack = Stack[Int]()
    stack = stack.push(1).push(2).push(3)
    assertEquals(stack.pop.map(_._1), Some(3))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(2))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(1))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop, None)

  test("stack: simple test 3"):
    var stack = Stack[Int]()
    stack = stack.push(10).push(20)
    assertEquals(stack.pop.map(_._1), Some(20))
    stack = stack.pop.map(_._2).get
    stack = stack.push(30).push(40)
    assertEquals(stack.pop.map(_._1), Some(40))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(30))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(10))

  test("stack: simple test 4"):
    var stack = Stack[Boolean]()
    stack = stack.push(true).push(false).push(true)
    assertEquals(stack.pop.map(_._1), Some(true))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(false))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(true))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop, None)

  test("stack: complex test 1"):
    var stack = Stack[Int]()
    for i <- 1 to 10 do
      stack = stack.push(i)
    assertEquals(stack.pop.map(_._1), Some(10))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(9))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(8))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(7))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(6))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(5))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(4))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(3))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(2))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some(1))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop, None)

  test("stack: complex test 2"):
    var stack = Stack[String]()
    stack = stack.push("hello")
    stack = stack.push("world")
    stack.pop match
      case Some(x, s) =>
        assertEquals(x, "world")
        stack = s.push("scala")
      case None => fail("Stack was empty")
    assertEquals(stack.pop.map(_._1), Some("scala"))
    stack = stack.pop.map(_._2).get
    assertEquals(stack.pop.map(_._1), Some("hello"))

  test("stack: complex test 3"):
    val numbers = List(15, 25, 35, 45, 55)
    var stack = numbers.foldLeft(Stack[Int]())(_.push(_))
    var popped = List[Int]()
    while !stack.isEmpty do
      stack.pop match
        case Some(x, s) =>
          popped = x :: popped
          stack = s
        case None => fail("Stack was empty")
    assertEquals(popped, numbers)

  test("stack: complex test 4"):
    var stack = Stack[Double]()
    stack = stack.push(1.1).push(2.2).push(3.3)
    var sum = 0.0
    while !stack.isEmpty do
      stack.pop match
        case Some(x, s) =>
          sum += x
          stack = s
        case None => fail("Stack was empty")
    assertEquals(sum, 6.6, 0.0001)

  test("stack: complex test 5"):
    var stack1 = Stack[Int]()
    var stack2 = Stack[Int]()
    for i <- 1 to 5 do
      stack1 = stack1.push(i)
      stack2 = stack2.push(i * 2)
    while !stack1.isEmpty && !stack2.isEmpty do
      (stack1.pop, stack2.pop) match
        case (Some((x1, s1)), Some((x2, s2))) =>
          assertEquals(x2, x1 * 2)
          stack1 = s1
          stack2 = s2
        case _ => fail("One of the stacks was empty")
