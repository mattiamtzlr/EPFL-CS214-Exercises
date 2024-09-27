package debugging

def forall(p: Int => Boolean)(t: IntTree): Boolean = t match
  case IntEmptyTree() => true
  case IntBranch(v, l, r) => p(v) && forall(p)(l) && forall(p)(r)
  
def isBST(t: IntTree): Boolean = t match
    case IntEmptyTree() => true
    case IntBranch(v0, l, r) =>
         forall(v1 => v0 > v1)(l)
      && forall(v1 => v0 < v1)(r)
      && isBST(l)
      && isBST(r)

def insert(t: IntTree, v1: Int): IntTree = t match
    case IntEmptyTree() => IntBranch(v1, IntEmptyTree(), IntEmptyTree())
    case IntBranch(v0, l, r) =>
      if v1 < v0 then IntBranch(v0, insert(l, v1), r)
      else if v1 > v0 then IntBranch(v0, l, insert(r, v1))
      else t
