package futures.parallel

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration.Inf

case class ParList[+T](data: List[T]):
  def map[U](f: T => U): ParList[U] =
    val futures = data.map(x => Future(f(x)))
    val result  = Await.result(Future.sequence(futures), Inf)
    ParList(result)

  def flatMap[U](f: T => ParList[U]): ParList[U] =
    ParList(map(f).data.map(_.data).flatten)

  def filter(p: T => Boolean): ParList[T] =
    flatMap: x =>
      if p(x) then ParList(List(x))
      else ParList(Nil)
