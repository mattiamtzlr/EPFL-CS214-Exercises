package ds

abstract class Trie[X]:
  def find(key: String): Option[X]
  def delete(key: String): Trie[X]
  def insert(key: String, value: X): Trie[X]

case class TrieImpl[X](next: Map[Char, TrieImpl[X]], value: Option[X]) extends Trie[X]:
  def find(key: String): Option[X] =
    if key.isEmpty then value
    else next.get(key.head) match
      case None       => None
      case Some(trie) => trie.find(key.tail)

  def delete(key: String): TrieImpl[X] =
    if key.isEmpty then this.copy(value = None)
    else next.get(key.head) match
      case None       => this
      case Some(trie) => this.copy(next = next.updated(key.head, trie.delete(key.tail)))

  def insert(key: String, value: X): TrieImpl[X] =
    if key.isEmpty then this.copy(value = Some(value))
    else
      val n = next.get(key.head).getOrElse(TrieImpl())
      this.copy(next = next.updated(key.head, n.insert(key.tail, value)))

object TrieImpl:
  def apply[X](): TrieImpl[X] =
    TrieImpl(Map.empty[Char, TrieImpl[X]], None)
