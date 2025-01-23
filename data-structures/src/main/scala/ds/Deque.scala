package ds

abstract class Deque[X]:
  /** Push an element to the front. */
  def pushFirst(elem: X): Deque[X]

  /** Pop an element from the front. */
  def popFirst: Option[(X, Deque[X])]

  /** Push an element to the back. */
  def pushLast(elem: X): Deque[X]

  /** Pop an element from the back. */
  def popLast: Option[(X, Deque[X])]

  /** Check whether the deque is empty. */
  def isEmpty: Boolean


// Implemented as two stacks back-to-back
case class DequeImpl[X](front: List[X], back: List[X]) extends Deque[X]:
  def pushFirst(elem: X): Deque[X] =
    DequeImpl(elem :: front, back)

  def popFirst: Option[(X, Deque[X])] =
    if isEmpty then None
    else front match
      case Nil => {
        val backRev = back.reverse
        Some((backRev.head, DequeImpl(backRev.tail, Nil)))
      }
      case h :: t => Some((h, DequeImpl(t, back)))

  def pushLast(elem: X): Deque[X] =
    DequeImpl(front, elem :: back)

  def popLast: Option[(X, Deque[X])] =
    if isEmpty then None
    else back match
      case Nil => {
        val frontRev = front.reverse
        Some((frontRev.head, DequeImpl(Nil, frontRev.tail)))
      }
      case h :: t => Some((h, DequeImpl(front, t)))

  def isEmpty: Boolean =
    front.isEmpty && back.isEmpty

object Deque:
  /** Create an empty deque. */
  def apply[X](): Deque[X] =
    DequeImpl[X](List.empty[X], List.empty[X])

extension [X](deque: Deque[X])
  def popFirstAll: List[X] = deque.popFirst match
    case None           => Nil
    case Some(x, deque) => x :: deque.popFirstAll

  def popLastAll: List[X] = deque.popLast match
    case None           => Nil
    case Some(x, deque) => x :: deque.popLastAll
