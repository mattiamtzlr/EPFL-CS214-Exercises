package ds

import munit.ScalaCheckSuite
import org.scalacheck.Prop.*

class TriePropTest extends ScalaCheckSuite:
  property("trie: insert and find"):
    forAll: (xs: Set[Int]) =>
      var t = TrieImpl[Int]()
      val kvs = xs.toList.map(x => (x.toString, x))
      kvs.foreach((k, v) => t = t.insert(k, v))
      kvs.forall((k, v) => t.find(k) == Some(v))

  property("trie: insert then insert"):
    forAll: (xs: Set[Int]) =>
      var t = TrieImpl[Int]()
      val kvs = xs.toList.map(x => (x.toString, x))
      kvs.foreach((k, v) => t = t.insert(k, v))
      kvs.foreach((k, v) => t = t.insert(k, v * v))
      kvs.forall((k, v) => t.find(k) == Some(v * v))

  property("trie: insert then delete"):
    forAll: (xs: Set[Int]) =>
      var t = TrieImpl[Int]()
      val kvs = xs.toList.map(x => (x.toString, x))
      kvs.foreach((k, v) => t = t.insert(k, v))
      kvs.foreach((k, _) => t = t.delete(k))
      kvs.forall((k, _) => t.find(k) == None)

  property("trie: delete nonexisting keys"):
    forAll: (xs: Set[Int]) =>
      var t = TrieImpl[Int]()
      val kvs = xs.toList.map(x => (x.toString, x))
      kvs.foreach((k, v) => t = t.insert(k, v))
      kvs.foreach((k, _) => t = t.delete(k + "A"))
      kvs.forall((k, v) => t.find(k) == Some(v))
