package futures.runtime

import scala.concurrent.{Await, Future}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

def sleep(durationSec: Int): Future[Unit] =
  Future(Thread.sleep(durationSec * 1000))

def wait[T](message: String)(f: Future[T]): Unit =
  val start = System.currentTimeMillis
  Await.result(f, Duration.Inf)
  val elapsed = (System.currentTimeMillis - start) / 1000.0
  println(f"    $message: ${elapsed}%.0f seconds")
