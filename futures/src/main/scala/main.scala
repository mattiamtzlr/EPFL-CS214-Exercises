import futures.ops.*

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global

import futures.rest.{HttpBackend, showContributors, showContributorsDirect, ConsoleWindow, Window, githubStubBackend}
import futures.friends.{
  makeFriendsWithCallback,
  makeFriendsMonadicFlatMap,
  makeFriendsMonadicFor,
  makeFriendsDirect,
  makeFriendsMonadicParallel,
  makeFriendsMonadicParallelFor,
  makeFriendsDirectParallel,
  MockDatabase
}
import futures.runtime.{sleep, wait}

def fail(msg: String): Unit =
  System.err.println(msg)
  System.exit(1)

@main def predictionMain(): Unit =
  println("Single operators:")

  println("  flatMap:")

  wait("2flatMap4"):
    sleep(2).flatMap(_ => sleep(4)) // 6
  wait("4flatMap2"):
    sleep(4).flatMap(_ => sleep(2)) // 6

  println("  zip:")
  wait("2 zip 4"):
    sleep(2).zip(sleep(4)) // 4
  wait("4 zip 2"):
    sleep(4).zip(sleep(2)) // 4

  println("  sequence (;):")
  wait("2 ; 4"):
    sleep(2); sleep(4) // 4
  wait("4 ; 2"):
    sleep(4); sleep(2) // 2

  println("Combinations:")

  wait("2flatMap4 zip 1flatMap6"):
    sleep(2).flatMap(_ => sleep(4))
      .zip(sleep(1).flatMap(_ => sleep(6))) // 7

  wait("2zip1 flatMap 4zip6"):
    sleep(2).zip(sleep(1)).flatMap(_ =>
      sleep(4).zip(sleep(6))
    ) // 8

  wait("2zip1 flatMap (4;6)"):
    sleep(2).zip(sleep(1)).flatMap(_ =>
      sleep(4); sleep(6)
    ) // 8

  wait("2zip1 flatMap (6;4)"):
    sleep(2).zip(sleep(1)).flatMap(_ =>
      sleep(6); sleep(4)
    ) // 6

  println("Early start:")

  wait("A=2 B=4 AflatMapB"):
    val a = sleep(2)
    val b = sleep(4)
    a.flatMap(_ => b) // 4

  wait("A=2 B=4 AzipB"):
    val a = sleep(2)
    val b = sleep(4)
    a.zip(b) // 4

@main def showContributorsMain(
    org: String,
    repo: String,
    perPage: Int
): Unit =
  // given HttpBackend = sttp.client4.HttpClientFutureBackend
  given HttpBackend = githubStubBackend
  given Window = ConsoleWindow
  println(f"Getting contributors of $org/$repo")
  Await.result(showContributorsDirect(org, repo, perPage), 2.seconds)

@main def makeFriendsMain(version: String): Unit =
  def printResult(start: Long, res: Any): Unit =
    val elapsed = (System.currentTimeMillis - start) / 1000.0
    println(f"Aihsa and Carlos are now friends!\nresult: $res\nCompleted in: ${elapsed}%.0f seconds")

  def blockUntilDone(start: Long, future: Future[Any]) =
    println("Waiting for future to complete...")
    printResult(System.currentTimeMillis, Await.result(future, 5.seconds))

  val db = MockDatabase()
  val start = System.currentTimeMillis
  version match
    case "callbacks" =>
      println("Using callbacks")
      makeFriendsWithCallback("aisha", "carlos", db)(printResult(start, _))
      println("Waiting for callback to complete...")
      Thread.sleep(5000) // too bad we don't have another way to wait for the callback to complete.
    case "monadic-flatmap" =>
      println("Using monadic style and flatMap")
      blockUntilDone(start, makeFriendsMonadicFlatMap("aisha", "carlos", db))
    case "monadic-for" =>
      println("Using monadic style and for")
      blockUntilDone(start, makeFriendsMonadicFor("aisha", "carlos", db))
    case "direct" =>
      println("Using monadic style and direct")
      blockUntilDone(start, makeFriendsDirect("aisha", "carlos", db))
    case "monadic-flatmap-par" =>
      println("Using monadic style and parallel (1)")
      blockUntilDone(start, makeFriendsMonadicParallel("aisha", "carlos", db))
    case "monadic-for-par" =>
      println("Using monadic style and parallel (2)")
      blockUntilDone(start, makeFriendsMonadicParallelFor("aisha", "carlos", db))
    case "direct-par" =>
      println("Using direct style and parallel")
      blockUntilDone(start, makeFriendsDirectParallel("aisha", "carlos", db))

@main def tasksMain(): Unit =
  futures.tasks.managerRepl()

@main def futuresTest(): Unit =
  import scala.concurrent.Future

  val f1 = Future { Thread.sleep(1000); println("Hello") }
  val f2 = f1.map1(_ => println("world"))

  val f3 = Future:
    Thread.sleep(2000)
    1
  val f4 = Future:
    Thread.sleep(1000)
    2
  f3.zip1(f4).map1: (a, b) =>
    println(s"Result: ${a + b}")

  val fs = (1 to 5).toList.map: i =>
    Future:
      Thread.sleep(1000 * i)
      println(s"Number $i is finished!")
      i
  race1(fs).map: i =>
    println(s"I know that $i will win")
