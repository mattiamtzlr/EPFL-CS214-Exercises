package parallelism

import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*

extension [A](seq: Seq[A])
  // version 1: using Threads
  def parMapThread[B](f: A => B): Seq[B] =
    val results = new Array[Any](seq.length)
    val threads = seq.zipWithIndex.map({
      case (a, i) => new Thread(() => {
        val res = f(a)
        results.synchronized {
          results(i) = res
        }
      })
    })
    threads.foreach(_.start())
    threads.foreach(_.join())
    results.asInstanceOf[Array[B]].toSeq

  // version 2: using Future
  def parMapFuture[B](f: A => B): Seq[B] =
    val futures: Seq[Future[B]] = seq.map(a => Future(f(a)))
    futures.map(Await.result(_, Duration.Inf))