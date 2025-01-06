package monads

class WithoutMonadsTests extends munit.FunSuite:

  test("WithoutMonads.combine on Ok(1) and Ok(2) should be Ok(3)") {
    import WithoutMonads.*
    assertEquals(combine(Ok(1), Ok(2), (a, b) => Ok(a + b)), Ok(3))
  }

  test("WithoutMonads.combine on Ok(1) and DivByZero should be DivByZero") {
    import WithoutMonads.*
    assertEquals(combine(Ok(1), DivByZero, (a, b) => Ok(a + b)), DivByZero)
  }

  test("WithoutMonads.combine on DivByZero and Ok(2) should be DivByZero") {
    import WithoutMonads.*
    assertEquals(combine(DivByZero, Ok(2), (a, b) => Ok(a + b)), DivByZero)
  }

  test("WithoutMonads.combine on DivByZero and DivByZero should be DivByZero") {
    import WithoutMonads.*
    assertEquals(combine(DivByZero, DivByZero, (a, b) => Ok(a + b)), DivByZero)
  }

  test(
    "WithoutMonads.eval on Add(Constant(1), Constant(2)) should be Constant(3)"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(eval(Add(Constant(1), Constant(2))), Ok(3))
  }

  test(
    "WithoutMonads.eval on Division(Constant(4), Constant(2)) should be Constant(2)"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(eval(Division(Constant(4), Constant(2))), Ok(2))
  }

  test(
    "WithoutMonads.eval on Division(Constant(4), Constant(0)) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(eval(Division(Constant(4), Constant(0))), DivByZero)
  }

  test(
    "WithoutMonads.eval on Division(Constant(4), Add(Constant(1), Constant(-1))) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      eval(Division(Constant(4), Add(Constant(1), Constant(-1)))),
      DivByZero
    )
  }

  test(
    "WithoutMonads.eval on Division(Constant(4), Division(Constant(1), Constant(0))) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      eval(Division(Constant(4), Division(Constant(1), Constant(0)))),
      DivByZero
    )
  }

  test(
    "WithoutMonads.eval on Division(Division(Constant(1), Constant(0)), Constant(1)) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      eval(Division(Division(Constant(1), Constant(0)), Constant(1))),
      DivByZero
    )
  }

  test("WithoutMonads.flatMap on Ok(1) and (a => Ok(a + 1)) should be Ok(2)") {
    import WithoutMonads.*
    assertEquals(flatMap(Ok(1), (a => Ok(a + 1))), Ok(2))
  }

  test(
    "WithoutMonads.flatMap on DivByZero and (a => Ok(a + 1)) should be DivByZero"
  ) {
    import WithoutMonads.*
    assertEquals(flatMap(DivByZero, (a => Ok(a + 1))), DivByZero)
  }

  test(
    "WithoutMonads.combineFlatMap on Ok(1) and Ok(2) with (a, b) => Ok(a + b) should be Ok(3)"
  ) {
    import WithoutMonads.*
    assertEquals(combineFlatMap(Ok(1), Ok(2), (a, b) => Ok(a + b)), Ok(3))
  }

  test(
    "WithoutMonads.combineFlatMap on Ok(1) and DivByZero with (a, b) => Ok(a + b) should be DivByZero"
  ) {
    import WithoutMonads.*
    assertEquals(
      combineFlatMap(Ok(1), DivByZero, (a, b) => Ok(a + b)),
      DivByZero
    )
  }

  test(
    "WithoutMonads.combineFlatMap on DivByZero and Ok(2) with (a, b) => Ok(a + b) should be DivByZero"
  ) {
    import WithoutMonads.*
    assertEquals(
      combineFlatMap(DivByZero, Ok(2), (a, b) => Ok(a + b)),
      DivByZero
    )
  }

  test(
    "WithoutMonads.evalWithoutCombine on Add(Constant(1), Constant(2)) should be Constant(3)"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(evalWithoutCombine(Add(Constant(1), Constant(2))), Ok(3))
  }

  test(
    "WithoutMonads.evalWithoutCombine on Division(Constant(4), Constant(2)) should be Constant(2)"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(evalWithoutCombine(Division(Constant(4), Constant(2))), Ok(2))
  }

  test(
    "WithoutMonads.evalWithoutCombine on Division(Constant(4), Constant(0)) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalWithoutCombine(Division(Constant(4), Constant(0))),
      DivByZero
    )
  }

  test(
    "WithoutMonads.evalWithoutCombine on Division(Constant(4), Add(Constant(1), Constant(-1))) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalWithoutCombine(Division(Constant(4), Add(Constant(1), Constant(-1)))),
      DivByZero
    )
  }

  test(
    "WithoutMonads.evalWithoutCombine on Division(Constant(4), Division(Constant(1), Constant(0))) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalWithoutCombine(
        Division(Constant(4), Division(Constant(1), Constant(0)))
      ),
      DivByZero
    )
  }

  test(
    "WithoutMonads.evalWithoutCombine on Division(Division(Constant(1), Constant(0)), Constant(1)) should be DivByZero"
  ) {
    import WithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalWithoutCombine(
        Division(Division(Constant(1), Constant(0)), Constant(1))
      ),
      DivByZero
    )
  }

class StillWithoutMonadsTests extends munit.FunSuite:

  test(
    "StillWithoutMonads.evalStateFul on Add(Constant(1), Constant(2)) should be (Constant(3), 1)"
  ) {
    import StillWithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(evalStateful(Add(Constant(1), Constant(2))), (Ok(3), 1))
  }

  test(
    "StillWithoutMonads.evalStateFul on Division(Constant(4), Constant(2)) should be (Constant(2), 0)"
  ) {
    import StillWithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(evalStateful(Division(Constant(4), Constant(2))), (Ok(2), 0))
  }

  test(
    "StillWithoutMonads.evalStateFul on Division(Constant(4), Constant(0)) should be (DivByZero, 0)"
  ) {
    import StillWithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalStateful(Division(Constant(4), Constant(0))),
      (DivByZero, 0)
    )
  }

  test(
    "StillWithoutMonads.evalStateFul on Division(Constant(4), Add(Constant(1), Constant(-1))) should be (DivByZero, 1)"
  ) {
    import StillWithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalStateful(Division(Constant(4), Add(Constant(1), Constant(-1)))),
      (DivByZero, 1)
    )
  }

  test(
    "StillWithoutMonads.evalStateFul on Division(Constant(4), Division(Constant(1), Constant(0))) should be (DivByZero, 0)"
  ) {
    import StillWithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalStateful(Division(Constant(4), Division(Constant(1), Constant(0)))),
      (DivByZero, 0)
    )
  }

  test(
    "StillWithoutMonads.evalStateFul on Add(Add(Constant(1),Constant(2)),Add(Division(Constant(4),Add(Constant(1),Constant(1))Add(Constant(1),Constant(1))))) should be (Constant(7), 5)"
  ) {
    import StillWithoutMonads.*
    import EvaluatorCommon.*

    assertEquals(
      evalStateful(
        Add(
          Add(Constant(1), Constant(2)),
          Add(
            Division(Constant(4), Add(Constant(1), Constant(1))),
            Add(Constant(1), Constant(1))
          )
        )
      ),
      (Ok(7), 5)
    )
  }

class WithMonadsTest extends munit.FunSuite:

  test("WithMonads.unit on 3 should be State => (Result, State)") {
    import WithMonads.*

    assertEquals(unit(3)(0), (3, 0))
  }

  test(
    "WithMonads.flatMap on unit(3) and (a => unitM(a + 1))(0) should be (4, 0)"
  ) {
    import WithMonads.*

    assertEquals(unit(3).flatMap((a => unit(a + 1)))(0), (4, 0))
  }

  test(
    "WithMonads.eval on Add(Constant(1), Constant(2)) on (0) should be (3, 1)"
  ) {
    import WithMonads.*
    import EvaluatorCommon.*

    assertEquals(eval(Add(Constant(1), Constant(2)))(0), (3, 1))
  }

  test(
    "WithMonads.eval on Division(Constant(4), Constant(2)) on (0) should be (2, 0)"
  ) {
    import WithMonads.*
    import EvaluatorCommon.*

    assertEquals(eval(Division(Constant(4), Constant(2)))(0), (2, 0))
  }

class LoggingEvaluatorTest extends munit.FunSuite:

  test("LoggingEvaluator.unit[Int] on 3 should be (3, Nil)") {
    import LoggingEvaluator.*

    assertEquals(unit[Int](3), (3, Nil))
  }

  test(
    "LoggingEvaluator.unit[String] on \"Hello\" should be (\"Hello\", Nil)"
  ) {
    import LoggingEvaluator.*

    assertEquals(unit[String]("Hello"), ("Hello", Nil))
  }

  test(
    "LoggingEvaluator.flatMap[Int, Int] on unit(3) and (a => (a+1, List(f\"$a + 1\"))) should be (4, List(\"3 + 1\"))"
  ) {
    import LoggingEvaluator.*

    assertEquals(
      unit[Int](3).flatMap((a => (a + 1, List(f"$a + 1")))),
      (4, List("3 + 1"))
    )
  }

  test(
    "LoggingEvaluator.flatMap[Int, String] on unit(3) and (a => (f\"$a + 1\", List(f\"$a + 1\"))) should be (\"3 + 1\", List(\"3 + 1\"))"
  ) {
    import LoggingEvaluator.*

    assertEquals(
      unit[Int](3).flatMap((a => (f"$a + 1", List(f"$a + 1")))),
      ("3 + 1", List("3 + 1"))
    )
  }

  test(
    "LoggingEvaluator.eval on Add(Constant(1), Constant(2)) should be (3, List(line(Constant(1), 1), line(Constant(2), 2), line(Add(Constant(1), Constant(2)), 3)))"
  ) {
    import LoggingEvaluator.*
    import EvaluatorCommon.*

    assertEquals(
      eval(Add(Constant(1), Constant(2))),
      (
        3,
        List(
          line(Constant(1), 1),
          line(Constant(2), 2),
          line(Add(Constant(1), Constant(2)), 3)
        )
      )
    )
  }

  test(
    "LoggingEvaluator.eval on Division(Constant(4), Constant(2)) should be (2, List(line(Constant(4), 1), line(Constant(2), 2), line(Division(Constant(4), Constant(2)), 3)))"
  ) {
    import LoggingEvaluator.*
    import EvaluatorCommon.*

    assertEquals(
      eval(Division(Constant(4), Constant(2))),
      (
        2,
        List(
          line(Constant(4), 4),
          line(Constant(2), 2),
          line(Division(Constant(4), Constant(2)), 2)
        )
      )
    )
  }
