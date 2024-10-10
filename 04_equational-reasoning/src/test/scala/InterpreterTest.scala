package lists

import lists.Instruction.*
import lists.Interpreter.*
import lists.NotEnoughOperandsInStackException

class InterpreterTests extends munit.FunSuite:

  val stack3: List[Int] = 3 :: 5 :: -2 :: Nil
  val stack5: List[Int] = 4 :: -9 :: 8 :: 2 :: 3 :: Nil
  val prog: Program = (
    Push(2) ::
      Push(5) :: Push(-3) :: Minus :: Mul ::
      Push(4) :: Push(9) :: Add :: Add :: Nil
  )

  test("interpInst: push to stack"):
    assertEquals(interpInst(stack3, Push(7)), 7 :: stack3)

  test("interpInst: pop from empty stack"):
    intercept[NotEnoughOperandsInStackException]:
      interpInst(Nil, Pop)

  test("interpInst: pop from non-empty stack"):
    assertEquals(interpInst(stack3, Pop), 5 :: -2 :: Nil)

  test("interpInst: add in empty stack"):
    intercept[NotEnoughOperandsInStackException]:
      interpInst(Nil, Add)

  test("interpInst: add in 1-element stack"):
    intercept[NotEnoughOperandsInStackException]:
      interpInst(1 :: Nil, Add)

  val add: (Int, Int) => Int = (x, y) => x + y
  val minus: (Int, Int) => Int = (x, y) => x - y
  val mul: (Int, Int) => Int = (x, y) => x * y
  val div: (Int, Int) => Int = (x, y) => x / y

  val operations = scala.List(
    ("add", Add, add),
    ("minus", Minus, minus),
    ("mul", Mul, mul),
    ("div", Div, div)
  )

  for (opName, opInst, opFunc) <- operations do
    test(s"interpInst: $opName in empty stack"):
      intercept[NotEnoughOperandsInStackException]:
        interpInst(Nil, opInst)

    test(s"interpInst: $opName in 1-element stack"):
      intercept[NotEnoughOperandsInStackException]:
        interpInst(1 :: Nil, opInst)

    test(s"interpInst: $opName in 5-element stack"):
      assertEquals(opFunc(stack5.tail.head, stack5.head) :: stack5.tail.tail, interpInst(stack5, opInst))

  test(s"interpInst: divide by zero exception"):
    intercept[DivideByZeroException]:
      interpInst(0 :: 9 :: Nil, Div)

  test("interpProg: program with initial empty stack"):
    assertEquals(interpProg(Nil, prog), 29 :: Nil)

  test("interpProg: program with stack5"):
    assertEquals(interpProg(stack5, Add :: Pop :: prog), 29 :: stack5.tail.tail)
