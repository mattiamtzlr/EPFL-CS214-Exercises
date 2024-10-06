import quadtree.*
import QuadTree.*

// In a 16 x 16 area => center: (7.5, 7.5)
// Inspired by assets/quadtree-example.png
/* 
x   0 1 2 3   4 5 6 7   8 9 a b   c d e f
  +---------+---------+---------+---------+
0 |                   |         |         |
1 |                   |         |       C |
2 |                   |         |   F     |        A = (5,   5)
3 |                   |         |         |        B = (2,   9)
  |                   +---------+---------+        C = (15,  1)
4 |                   |         |         |        D = (9,   7)
5 |             A     |         |         |        E = (14, 15)
6 |                   |         |         |        F = (13,  2)
7 |                   |   D     |         |
  +---------+---------+---------+---------+
8 |                   |         |         |
9 |     B             |         |         |
a |                   |         |         |
b |                   |         |         |
  |                   +---------+---------+
c |                   |         |    |    |
d |                   |         |____|____|
e |                   |         |    |    |
f |                   |         |    |E   |
  +---------------------------------------+
*/
val quadTree: QuadTree[Char] = Quad(
    Vector2(7.5, 7.5), 
    nw = Leaf(List(WithPos(Vector2(5, 5), 'A'))),
    ne = Quad(
        Vector2(11.5, 3.5), 
        Empty, Leaf(List(WithPos(Vector2(15, 1), 'C'), WithPos(Vector2(13, 2), 'F'))),
        Leaf(List(WithPos(Vector2(9, 7), 'D'))), Empty
    ),
    sw = Leaf(List(WithPos(Vector2(2, 9), 'B'))),
    se = Quad(
        Vector2(11.5, 11.5), Empty, Empty, Empty, Quad(
            Vector2(13.5, 13.5), Empty, Empty, Empty, Leaf(List(WithPos(Vector2(14, 15), 'E')))
        )
    )
)

quadTree.size
quadTree.contains(WithPos(Vector2(5, 5), 'A'))

// Insert element in SW leaf of SE quadtree
val inserted = quadTree.insert(WithPos(Vector2(10, 12), 'G'))
inserted.size
inserted.contains(WithPos(Vector2(10, 12), 'G'))