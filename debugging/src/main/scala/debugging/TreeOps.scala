package debugging

def isBST(t: IntTree): Boolean =
  t match
    case IntEmptyTree() => true
    case IntBranch(v, l, r) =>
         (l == IntEmptyTree() || v > l.value)
      && (r == IntEmptyTree() || v < r.value)
      && isBST(l)
      && isBST(r)

def insert(t: IntTree, v1: Int): IntTree =
  t match
    case IntEmptyTree() => IntBranch(v1, IntEmptyTree(), IntEmptyTree())
    case IntBranch(v0, l, r) =>
      if v1 < v0 then IntBranch(v0, insert(l, v1), r)
      else IntBranch(v0, l, insert(r, v1))
