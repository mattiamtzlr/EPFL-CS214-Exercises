package futures.ops

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.concurrent.ExecutionContext

extension [T](self: Future[T])
  def map1[U](f: T => U): Future[U] =
    val promise = Promise[U]()
    self.onComplete:
      case Success(t) => promise.complete(Try(f(t)))
      case Failure(e) => promise.complete(Failure(e))

    promise.future


extension [T](self: Future[T])
  def flatMap1[U](f: T => Future[U]): Future[U] =
    val promise = Promise[U]()
    self.onComplete: res =>
      res.flatMap(t => Try(f(t))) match
        case Success(v) => v.onComplete(promise.complete)
        case Failure(e) => promise.complete(Failure(e))

    promise.future


extension [T](self: Future[T])
  def zip1[U](other: Future[U]): Future[(T, U)] =
    self.flatMap1(t1 => other.map(t2 => (t1, t2)))


def sequence1[T](futures: List[Future[T]]): Future[List[T]] =
  futures match
    case h :: t => h.flatMap1(h1 => sequence1(t).map(t1 => h1 :: t1))
    case Nil    => Future(Nil)


def race1[T](futures: List[Future[T]]): Future[T] =
  val promise = Promise[T]()
  futures.foreach(_.onComplete(r => if !promise.isCompleted then promise.complete(r)))
  promise.future