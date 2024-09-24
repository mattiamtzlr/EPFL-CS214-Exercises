import patmat.*
import patmat.IntTree.*
import patmat.IntTreeOps.*

val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
val mapped = treeMap(tree, x => x + 1)
val reduced = treeReduce(mapped, (a, b) => a + b)
