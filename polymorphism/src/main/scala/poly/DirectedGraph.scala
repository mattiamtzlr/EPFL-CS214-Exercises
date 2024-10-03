package poly

import poly.MyList.*

type NodeId = Int
type DirectedEdge = (NodeId, NodeId)
type DirectedGraph = MyList[DirectedEdge]
type Triangle = (NodeId, NodeId, NodeId)

def triangles(edges: DirectedGraph): MyList[Triangle] = edges match
  case Nil => Nil
  case Cons(e1, Cons(e2, Cons(e3, t))) => 
    if e1._2 == e2._1 && e2._2 == e3._1 && e3._2 == e1._1 
    then Cons((e1._1, e2._1, e3._1), triangles(t))
    else triangles(Cons(e2, Cons(e3, t)))
  case _   => Nil