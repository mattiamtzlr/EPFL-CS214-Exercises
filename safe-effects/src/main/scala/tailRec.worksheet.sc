import tailRecursion.lists.*

val l1 = List(1, 2, 3)
val l2 = List(4, 5, 6)

reverseAppend(l1, l2)
reverseAppendLoop(l1, l2)

sum(l1)
sumLoop(l1)
sum(l2)
sumLoop(l2)

foldLeft(l1, 1)(_ * _)
foldLeftLoop(l1, 1)(_ * _)
foldLeftForeach[Int, Int](1)(_ * _)(l1)
foldLeft(l2, 1)(_ * _)
foldLeftLoop(l2, 1)(_ * _)
foldLeftForeach[Int, Int](1)(_ * _)(l2)

reverseAppendLoop(l1, l2).foldt(0)(_ + _)

val l3 = List("apple", "banana", "sand", "orange", "brown", "six", "other", "balcony", "acorn")

l3.groupBy(s => s.head)
groupByForeach[String, Char](s => s.head)(l3)
groupByFoldRight[String, Char](s => s.head)(l3)

import tailRecursion.trees.*

import Tree.*
val t1 = Node(Leaf(4), Node(Leaf(2), Node(Leaf(7), Leaf(1))))
val t2 = Node(Node(Leaf(8), Node(Leaf(7), Leaf(2))), Node(Leaf(3), Leaf(5)))

sizeRec(t1)
sizeLoop(t1)

sizeRec(t2)
sizeLoop(t2)

isRightLineTree(t1)
sumRightLineTree(t1)

isRightLineTree(t2)
sumRotate(t2, 0)

tailRecursion.trees.sumLoop(t1)
tailRecursion.trees.sumLoop(t2)

val stack = MStack(List(3, 7, 2))
stack.push(5)
stack.size
stack.contains(5)
stack.pop()
stack.pop()
stack.pop()
stack.pop()
stack.isEmpty
stack.size

postOrderTraversal(t1)
postOrderTraversal(t2)

reduceLoop(t1, _ * _)
reduceLoop(t2, _ * _)

mapLoop(t1, i => (i + 97).toChar)
mapLoop(t2, i => i % 2 == 0)