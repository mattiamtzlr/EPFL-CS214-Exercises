package monads

import scala.language.higherKinds

trait Monad[M[_]]:
  def unit[A](a: A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

trait Monoid[A]:
  def munit: A
  def mcombine(x: A)(y: A): A

case class IdentityM[A](a: A)

object IdentityMonad extends Monad[IdentityM]:
  def unit[A](a: A): IdentityM[A] =
    ???

  def flatMap[A, B](ma: IdentityM[A])(f: A => IdentityM[B]): IdentityM[B] =
    ???

enum OptionM[+A]:
  case Some(a: A)
  case None

import OptionM.*

object OptionMonad extends Monad[OptionM]:
  def unit[A](a: A): OptionM[A] =
    ???

  def flatMap[A, B](ma: OptionM[A])(f: A => OptionM[B]): OptionM[B] =
    ???

case class StateM[S, A](run: S => (S, A))

object StateM:
  // Get state at this point
  def get[S](): StateM[S, S] =
    StateM(s => (s, s))

  // Replace the state
  def put[S](s: S): StateM[S, Unit] =
    StateM(_ => (s, ()))

  // Update the state
  def modify[S](f: S => S): StateM[S, Unit] =
    StateM((s: S) => (f(s), ()))

class StateMonad[S] extends Monad[[A] =>> StateM[S, A]]:
  def unit[A](a: A): StateM[S, A] =
    ???

  def flatMap[A, B](ma: StateM[S, A])(f: A => StateM[S, B]): StateM[S, B] =
    ???

case class ReaderM[R, A](run: R => A)

object ReaderM:
  // Get current context
  def ask[R](): ReaderM[R, R] =
    ReaderM(r => r)

  // Do operation in modified context
  def local[R, A](f: (R => R), m: ReaderM[R, A]): ReaderM[R, A] =
    ReaderM(r => m.run(f(r)))

class ReaderMonad[R] extends Monad[[A] =>> ReaderM[R, A]]:
  def unit[A](a: A): ReaderM[R, A] =
    ???

  def flatMap[A, B](ma: ReaderM[R, A])(f: A => ReaderM[R, B]): ReaderM[R, B] =
    ???

case class WriterM[W, A](run: (W, A))

class WriterMonad[W](m: Monoid[W]) extends Monad[[A] =>> WriterM[W, A]]:
  def unit[A](a: A): WriterM[W, A] =
    ???

  def flatMap[A, B](ma: WriterM[W, A])(f: A => WriterM[W, B]): WriterM[W, B] =
    ???
