package debugging

class TreeOpsTests extends munit.FunSuite:

  val tree1: IntTree =
    IntBranch(
      3,
      IntBranch(2, IntEmptyTree(), IntEmptyTree()),
      IntBranch(4, IntEmptyTree(), IntEmptyTree())
    )

  val tree2: IntTree =
    IntBranch(
      5,
      IntBranch(2, IntBranch(1, IntEmptyTree(), IntEmptyTree()), IntBranch(4, IntEmptyTree(), IntEmptyTree())),
      IntBranch(10, IntBranch(7, IntEmptyTree(), IntEmptyTree()), IntBranch(19, IntEmptyTree(), IntEmptyTree()))
    )

  val tree3: IntTree =
    IntBranch(
      5,
      IntBranch(2, IntBranch(1, IntEmptyTree(), IntEmptyTree()), IntBranch(4, IntEmptyTree(), IntEmptyTree())),
      IntBranch(10, IntBranch(4, IntEmptyTree(), IntEmptyTree()), IntBranch(19, IntEmptyTree(), IntEmptyTree()))
    )

  test("isBST: on tree1"):
    assertEquals(isBST(tree1), true)

  test("isBST: on tree2"):
    assertEquals(isBST(tree2), true)

  test("isBST: on tree3"):
    assertEquals(isBST(tree3), false)

  test("insert: insert 0 on tree2"):
    assertEquals(
      insert(tree2, 0),
      IntBranch(
        5,
        IntBranch(
          2,
          IntBranch(1, IntBranch(0, IntEmptyTree(), IntEmptyTree()), IntEmptyTree()),
          IntBranch(4, IntEmptyTree(), IntEmptyTree())
        ),
        IntBranch(
          10,
          IntBranch(7, IntEmptyTree(), IntEmptyTree()),
          IntBranch(19, IntEmptyTree(), IntEmptyTree())
        )
      )
    )

  test("insert: insert 6 on tree2"):
    assertEquals(
      insert(tree2, 6),
      IntBranch(
        5,
        IntBranch(
          2,
          IntBranch(1, IntEmptyTree(), IntEmptyTree()),
          IntBranch(4, IntEmptyTree(), IntEmptyTree())
        ),
        IntBranch(
          10,
          IntBranch(7, IntBranch(6, IntEmptyTree(), IntEmptyTree()), IntEmptyTree()),
          IntBranch(19, IntEmptyTree(), IntEmptyTree())
        )
      )
    )
