package exceptions

extension [T](l: List[T])
  def containsRec(t0: T): Boolean =
    l match
      case Nil      => false
      case hd :: tl => hd == t0 || tl.containsRec(t0)

extension [T](l: List[T])
  def containsFold(t0: T): Boolean =
    l.foldRight(false)((hd, found) => found || hd == t0)

extension [T](l: List[T])
  def containsExn(t0: T): Boolean =
    case object Found extends Exception
    try
      for e <- l if e == t0 do throw Found
      false
    catch
      case Found => true


import scala.util.boundary

extension [T](l: List[T])
  def containsBoundary(t0: T): Boolean =
    ???

extension [T](l: List[T])
  def findExn(p: T => Boolean): Option[T] =
    ???

extension [T](l: List[T])
  def findBoundary(p: T => Boolean): Option[T] =
    ???
