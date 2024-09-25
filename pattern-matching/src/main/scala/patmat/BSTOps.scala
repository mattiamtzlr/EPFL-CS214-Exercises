package patmat

import BSTContext.*

object BSTOps:
  import EnumContext.LookupResult
  import LookupResult.*

  def lookup(bst: BSTContext, key: String): LookupResult = bst match
    case Leaf => NotFound
    case Branch(k, v, l, r) =>
      if key < k then lookup(l, key) 
      else if key > k then lookup(r, key)
      else Ok(v)

  def insert(bst: BSTContext, key: String, value: Int): BSTContext = bst match
    case Leaf => Branch(key, value, Leaf, Leaf)
    case Branch(k, v, l, r) => 
      if key < k then Branch(k, v, insert(l, key, value), r)
      else if key > k then Branch(k, v, l, insert(r, key, value))
      else Branch(k, value, l, r)
    
  def rotateLeft(tree: BSTContext): BSTContext =
    ???

  def rotateRight(tree: BSTContext): BSTContext = 
    ???