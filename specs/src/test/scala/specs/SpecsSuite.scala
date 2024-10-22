package specs

class CombinationsSuite extends munit.FunSuite:

    val letters = Set('a', 'b', 'c', 'd')

    test("combinations of an empty set returns an empty set"):
        assertEquals(combinations(Set.empty, 1), Set.empty)
        assertEquals(combinations(Set.empty, 5), Set.empty)
    
    test("combinations of any set with k = 0 returns a singleton of the emtpy set"):
        assertEquals(combinations(Set.empty, 0), Set(Set.empty))
        assertEquals(combinations(letters, 0), Set(Set.empty))

    val outputs = List(
        Set('a', 'b', 'c', 'd').map(Set(_)),
        Set(
            Set('a', 'b'), Set('a', 'c'), Set('a', 'd'),
            Set('b', 'c'), Set('b', 'd'), Set('c', 'd')
        ),
        Set(
            Set('a', 'b', 'c'), Set('a', 'b', 'd'), Set('a', 'c', 'd'), Set('b', 'c', 'd')
        ),
        Set(Set('a', 'b', 'c', 'd'))
    )
    for i <- 1 to letters.size
    yield {
        test(s"combinations correctly generates combinations of {a, b, c, d} of length $i:"):
            assertEquals(combinations(letters, i), outputs(i-1))
    }