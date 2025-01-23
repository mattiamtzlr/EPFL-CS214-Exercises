package ds

// FILO - First In, Last Out
abstract class Stack[X]:
  /** Push an element into the stack. */
  def push(elem: X): Stack[X]

  /** Pop an element from the stack. Return None if the stack is empty. */
  def pop: Option[(X, Stack[X])]

  /** Check whether the stack is empty. */
  def isEmpty: Boolean


case class StackImpl[X](data: List[X]) extends Stack[X]:
  def push(elem: X): Stack[X] =
    StackImpl(elem :: data)

  def pop: Option[(X, Stack[X])] =
    if isEmpty then None
    else Some((data.head, StackImpl(data.tail)))

  def isEmpty: Boolean =
    data.isEmpty

object Stack:
  /** Create an empty stack. */
  def apply[X](): Stack[X] =
    StackImpl(List.empty[X])
