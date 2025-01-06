package monads

import scala.util.control.NonFatal

enum TryM[+A]:
  case Success(a: A)
  case Failure(ex: Throwable)
import TryM.*

object TryMonadPure extends Monad[TryM]:
  def unit[A](a: A): TryM[A] =
    ???

  def flatMap[A, B](ma: TryM[A])(f: A => TryM[B]): TryM[B] =
    ???

object TryMonadEffectful:
  def unit[A](a: => A): TryM[A] =
    ???

  def flatMap[A, B](ma: TryM[A])(f: A => TryM[B]): TryM[B] =
    ???
