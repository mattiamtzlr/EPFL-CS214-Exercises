package specs

class CombinationsSuite extends munit.FunSuite:
    test("badCombinations correctly handles empty sets"):
        assertEquals(badCombinations(Set(), 0), Set(Set()))