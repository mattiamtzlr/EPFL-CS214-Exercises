package debugging

import java.nio.charset.Charset
import java.util.Locale

class EncodingSuite extends munit.FunSuite:
  test("`findPairs` rejects unrelated inputs"):
    assertEquals(findPairs("a", "z").toList, Nil)
    assertEquals(findPairs("é", "«å").toList, Nil)

  test("`findPairs`: UTF-8 / Latin"):
    assertExists(("UTF-8", "ISO-8859-1"), charsetPairHasName):
      findPairs("Genève Aéroport", "GenÃ¨ve AÃ©roport")
    assertExists(("UTF-8", "ISO-8859-1"), charsetPairHasName):
      findPairs("Neuchâtel", "NeuchÃ¢tel")

    assertExists(("UTF-8", "ISO-8859-9"), charsetPairHasName):
      findPairs("Genève Aéroport", "GenÃ¨ve AÃ©roport")
    assertExists(("UTF-8", "ISO-8859-15"), charsetPairHasName):
      findPairs("Neuchâtel", "NeuchÃ¢tel")

  test("`findPairs`: UTF-8 / UTF-16"):
    assertExists(("UTF-8", "UTF-16"), charsetPairHasName):
      findPairs("Neuchâtel", "乥畣棃ꉴ敬")

  test("`findPairs`: UTF-8 / Devanagari"):
    assertExists(("UTF-8", "x-ISCII91"), charsetPairHasName):
      findPairs("Neuchâtel", "Neuchथंtel")
    assertExists(("UTF-8", "x-ISCII91"), charsetPairHasName):
      findPairs("Genève Aéroport", "Genथउve Aथऊroport")

  def assertExists[T, Q](q: Q, eq: (T, Q) => Boolean)(l: Iterable[T]): Unit =
    assert(l.exists(eq(_, q)), f"${q} not found in ${l}")

  def charsetPairHasName(p1: (Charset, Charset), p2: (String, String)): Boolean =
    p1._1.name() == p2._1 &&
      p1._2.name() == p2._2
