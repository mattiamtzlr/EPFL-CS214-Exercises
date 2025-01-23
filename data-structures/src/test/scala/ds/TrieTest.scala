package ds

class TrieTest extends munit.FunSuite:
  test("trie: empty find"):
    assertEquals(TrieImpl[Int]().find("abc"), None)
    assertEquals(TrieImpl[Int]().find(""), None)

  test("trie: insert then find"):
    assertEquals(TrieImpl[Int]().insert("42", 42).find("42"), Some(42))

  test("trie: delete then find"):
    assertEquals(TrieImpl[Int]().insert("42", 42).delete("42").find("42"), None)

  test("trie: insert more"):
    var tree = TrieImpl[Int]()
    tree = tree.insert("hello", 0)
    assertEquals(tree.find("hello"), Some(0))
    assertEquals(tree.find("helloworld"), None)
    assertEquals(tree.find("he"), None)
    tree = tree.insert("hey", 1)
    tree = tree.insert("he", 2)
    assertEquals(tree.find("hello"), Some(0))
    assertEquals(tree.find("hey"), Some(1))
    assertEquals(tree.find("he"), Some(2))
    tree = tree.delete("he")
    assertEquals(tree.find("hello"), Some(0))
    assertEquals(tree.find("hey"), Some(1))
    assertEquals(tree.find("he"), None)

  test("trie: simple test 1"):
    var trie = TrieImpl[Int]()
    trie = trie.insert("cat", 1)
    trie = trie.insert("car", 2)
    assertEquals(trie.find("cat"), Some(1))
    assertEquals(trie.find("car"), Some(2))
    assertEquals(trie.find("cap"), None)

  test("trie: simple test 2"):
    var trie = TrieImpl[Int]()
    trie = trie.insert("", 42)
    assertEquals(trie.find(""), Some(42))
    trie = trie.delete("")
    assertEquals(trie.find(""), None)

  test("trie: simple test 3"):
    var trie = TrieImpl[String]()
    trie = trie.insert("hello", "world")
    trie = trie.insert("help", "me")
    assertEquals(trie.find("hello"), Some("world"))
    assertEquals(trie.find("help"), Some("me"))
    assertEquals(trie.find("hell"), None)

  test("trie: complex test 1"):
    var trie = TrieImpl[Int]()
    val words = List("a", "ab", "abc", "abcd", "abcde")
    words.zipWithIndex.foreach((word, i) => trie = trie.insert(word, i))
    words.zipWithIndex.foreach((word, i) => assertEquals(trie.find(word), Some(i)))
    words.foreach(word => trie = trie.delete(word))
    words.foreach(word => assertEquals(trie.find(word), None))

  test("trie: complex test 2"):
    var trie = TrieImpl[Int]()
    val words = List("share", "shell", "sheriff", "sharp", "shear")
    words.zipWithIndex.foreach((word, i) => trie = trie.insert(word, i))
    words.zipWithIndex.foreach((word, i) => assertEquals(trie.find(word), Some(i)))
    assertEquals(trie.find("she"), None)
    assertEquals(trie.find("shared"), None)

  test("trie: complex test 3"):
    var trie = TrieImpl[Boolean]()
    trie = trie.insert("true", true)
    trie = trie.insert("false", false)
    assertEquals(trie.find("true"), Some(true))
    assertEquals(trie.find("false"), Some(false))
    trie = trie.delete("true")
    assertEquals(trie.find("true"), None)
    assertEquals(trie.find("false"), Some(false))

  test("trie: complex test 4"):
    var trie = TrieImpl[Int]()
    val numbers = (1 to 100).map(_.toString)
    numbers.zipWithIndex.foreach((num, i) => trie = trie.insert(num, i))
    numbers.zipWithIndex.foreach((num, i) => assertEquals(trie.find(num), Some(i)))
    assertEquals(trie.find("0"), None)
    assertEquals(trie.find("101"), None)
