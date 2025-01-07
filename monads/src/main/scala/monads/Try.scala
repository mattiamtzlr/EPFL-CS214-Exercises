package monads

import scala.util.control.NonFatal

enum TryM[+A]:
  case Success(a: A)
  case Failure(e: Throwable)
import TryM.*

object TryMonadPure extends Monad[TryM]:
  def unit[A](a: A): TryM[A] =
    Success(a)

  def flatMap[A, B](ma: TryM[A])(f: A => TryM[B]): TryM[B] =
    ma match
      case Success(a) => f(a)
      case Failure(e) => Failure(e)


object TryMonadEffectful:
  def unit[A](a: => A): TryM[A] =
    try Success(a)
    catch case e: Throwable => Failure(e)

  def flatMap[A, B](ma: TryM[A])(f: A => TryM[B]): TryM[B] =
    ma match
      case Success(a) =>
        try f(a)
        catch case e: Throwable => Failure(e)
      case Failure(e) => Failure(e)
