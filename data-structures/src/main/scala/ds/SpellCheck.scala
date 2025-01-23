package ds

/** Removes diacritics and all non-alphabetic characters from `str`. */
def normalizeString(str: String): String =
  java.text.Normalizer
    .normalize(str, java.text.Normalizer.Form.NFD)
    .replaceAll("\\p{InCombiningDiacriticalMarks}+", "")
    .replaceAll("[^a-zA-Z ]+", "")
    .toLowerCase(java.util.Locale.ROOT)
    .ensuring(_.forall(c => 'a' <= c && c <= 'z'))

/** Split string by whitespaces and `-`s. */
def splitString(c: String): List[String] =
  c.strip.split("[\\s\\-]+").map(normalizeString).filter(_ != "").toList

type Dict = List[String]

/** Tokenize the text. Returns a list of words that have been normalized. */
def tokenize(text: String): List[String] =
  splitString(text)

trait SpellChecker:
  /** Spell-check the input text. Return a list of misspelled words. */
  def checkText(text: String): List[String] =
    tokenize(text).filterNot(checkWord)

  /** Check whether the `word` is in `dict`. */
  def checkWord(word: String): Boolean

class TrieSpellChecker(dict: Dict) extends SpellChecker:
  val trie: Trie[Unit] =
    dict.foldRight(TrieImpl[Unit]())((word, trie) => trie.insert(word, ()))

  def checkWord(word: String): Boolean = trie.find(word).isDefined
