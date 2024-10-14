package parallelism

import scala.collection.parallel.CollectionConverters.*

class MapThread[A, B](seq: Seq[A], a: A, f: A => B) extends Thread:
  override def run(): Unit = ???


extension [A](seq: Seq[A])
  def parMap[B](f: A => B): Seq[B] =
    ???
