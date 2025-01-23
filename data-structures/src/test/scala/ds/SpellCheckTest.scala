package ds

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import cs214.PathMacro

object Assets:
  val thisFilePath = Paths.get(PathMacro.sourcePath)
  val assetsPath = thisFilePath.getParent.getParent.getParent.getParent.getParent.resolve("assets")
  val alicePath = assetsPath.resolve("alice_in_wonderland.txt").toString
  val princePath = assetsPath.resolve("le_petit_prince.txt").toString

  val wordListsPath = assetsPath.resolve("wordlists")
  val enDictPath: Map[String, String] =
    List("en-able.txt", "en-debian.txt", "en-ngsl.txt").map(n => n -> wordListsPath.resolve(n).toString).toMap
  val frDictPath: Map[String, String] =
    List("fr-edu.txt", "fr-lp.txt").map(n => n -> wordListsPath.resolve(n).toString).toMap

  def readFileContent(path: String): String =
    new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8)

  lazy val enDicts: Map[String, Dict] =
    enDictPath.map((n, p) => (n, tokenize(readFileContent(p))))

  lazy val frDicts: Map[String, Dict] =
    frDictPath.map((n, p) => (n, tokenize(readFileContent(p))))

  lazy val aliceText: String = readFileContent(alicePath)
  lazy val princeText: String = readFileContent(princePath)

class SpellCheckTest extends munit.FunSuite:
  test("spellcheck: simple example 1"):
    val dict = List("the", "red", "fox")
    val text = "the red fox jumps over"
    val checker = TrieSpellChecker(dict)
    assertEquals(checker.checkText(text), List("jumps", "over"))

  test("spellcheck: simple example 2"):
    val dict = List.empty[String]
    val text = "the red fox jumps over"
    val checker = TrieSpellChecker(dict)
    assertEquals(checker.checkText(text), List("the", "red", "fox", "jumps", "over"))

  test("spellcheck: simple example 3"):
    val dict = List("the", "red", "fox", "jumps", "over")
    val text = "the red fox jumps over"
    val checker = TrieSpellChecker(dict)
    assertEquals(checker.checkText(text), List())

  val largeDictSpecs: List[(String, Map[String, List[String]])] = List(
    (
      "hello world",
      Map(
        "en-able.txt" -> List(),
        "en-debian.txt" -> List(),
        "en-ngsl.txt" -> List()
      )
    ),
    (
      "bonjour",
      Map(
        "en-able.txt" -> List("bonjour"),
        "en-debian.txt" -> List("bonjour"),
        "en-ngsl.txt" -> List("bonjour")
      )
    ),
    (
      "let us develop a spell checker, shall we?",
      Map(
        "en-able.txt" -> List("a"),
        "en-debian.txt" -> List(),
        "en-ngsl.txt" -> List("checker")
      )
    ),
    (
      "a spell checker is being developped!",
      Map(
        "en-able.txt" -> List("a", "developped"),
        "en-debian.txt" -> List("developped"),
        "en-ngsl.txt" -> List("checker", "developped")
      )
    ),
    (
      "thirty, fourty, fivety",
      Map(
        "en-able.txt" -> List("fourty", "fivety"),
        "en-debian.txt" -> List("fourty", "fivety"),
        "en-ngsl.txt" -> List("thirty", "fourty", "fivety")
      )
    ),
    (
      "ok, okk, okkk",
      Map(
        "en-able.txt" -> List("ok", "okk", "okkk"),
        "en-debian.txt" -> List("ok", "okk", "okkk"),
        "en-ngsl.txt" -> List("okk", "okkk")
      )
    )
  )

  val aliceSpecs: Map[String, Int] = Map(
    "en-able.txt" -> 1968,
    "en-debian.txt" -> 855,
    "en-ngsl.txt" -> 3144
  )

  val princeSpecs: Map[String, Int] = Map(
    "fr-edu.txt" -> 5550,
    "fr-lp.txt" -> 1053
  )

  for (name, dict) <- Assets.enDicts do
    val checker = TrieSpellChecker(dict)
    for (text, answers) <- largeDictSpecs do
      test(f"spellcheck: checking `$text` with the dictionary from `$name`"):
        assertEquals(checker.checkText(text), answers(name))

    test(f"spellcheck: `alice_in_wonderland.txt` with the dictionary from `$name`"):
      val result = checker.checkText(Assets.aliceText)
      assertEquals(result.length, aliceSpecs(name))

  for (name, dict) <- Assets.frDicts do
    val checker = TrieSpellChecker(dict)
    test(f"spellcheck: `le_petit_prince.txt` with the dictionary from `$name`"):
      val result = checker.checkText(Assets.princeText)
      assertEquals(result.length, princeSpecs(name))
