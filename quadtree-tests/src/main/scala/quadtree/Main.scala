package quadtree

object Main {
  def main(args: Array[String]): Unit =
    import quadtree.*
    import QuadTree.*

    // In a 16 x 16 area => center: (7.5, 7.5)
    // Inspired by assets/quadtree-example.png
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

    /* 
    x   0 1 2 3   4 5 6 7   8 9 a b   c d e f
      +-------------------+---------+---------+
    0 |                   |         |         |        quadTree
    1 |                   |         |       C |
    2 |                   |         |   F     |        A = (5,   5)
    3 |                   |         |         |        B = (2,   9)
      |                   |---------+---------|        C = (15,  1)
    4 |                   |         |         |        D = (9,   7)
    5 |             A     |         |         |        E = (14, 15)
    6 |                   |         |         |        F = (13,  2)
    7 |                   |   D     |         |
      +-------------------+---------+---------+
    8 |                   |         |         |
    9 |     B             |         |         |
    a |                   |         |         |
    b |                   |         |         |
      |                   |---------+----+----|
    c |                   |         |    |    |
    d |                   |         |____|____|
    e |                   |         |    |    |
    f |                   |         |    |E   |
      +----------------------------------+----+
    */

    // Insert element in NE leaf of NE quadtree
    val inserted1 = quadTree.insert(WithPos(Vector2(12, 0), 'G'))

    // do it again to reach max capacity for that leaf
    val inserted2 = inserted1.insert(WithPos(Vector2(15, 3), 'H'))

    // and again to trigger the creation of a new Quad
    val inserted3 = inserted2.insert(WithPos(Vector2(14, 2), 'I'))
    /* 
    x   0 1 2 3   4 5 6 7   8 9 a b   c d e f
      +-------------------+---------+---------+
    0 |                   |         | G    |  |        inserted3
    1 |                   |         |______|C_|
    2 |                   |         |   F I|  |        A = (5,   5)
    3 |                   |         |      |H |        B = (2,   9)
      |                   |---------+---------|        C = (15,  1)
    4 |                   |         |         |        D = (9,   7)
    5 |             A     |         |         |        E = (14, 15)
    6 |                   |         |         |        F = (13,  2)
    7 |                   |   D     |         |        G = (12,  0)
      +-------------------+---------+---------+        H = (15,  3)
    8 |                   |         |         |        I = (14,  2)
    9 |     B             |         |         |
    a |                   |         |         |
    b |                   |         |         |
      |                   |---------+----+----|
    c |                   |         |    |    |
    d |                   |         |____|____|
    e |                   |         |    |    |
    f |                   |         |    |E   |
      +----------------------------------+----+
    */

    println(inserted3)

    // should only keep A, D (directly within region) and B (node within region)
    val filteredRegion = inserted3.filter(3, 10, 2, 13, _ => true)
    println(filteredRegion)

    // should only keep D, F, H as they are within the region and their ascii code is even
    val filteredPred = inserted3.filter(7.5, 15, 0, 7.5, _ % 2 == 0)
    println(filteredPred)

    // generates a QuadTree where every number between 0 and 63 inc is located as in a 
    // row-major matrix
    val elements = (0 to 63).toList
    val generated = fromCollection(elements, e => Vector2(e % 8, math.floor(e / 8)))
    //println(generated)
}