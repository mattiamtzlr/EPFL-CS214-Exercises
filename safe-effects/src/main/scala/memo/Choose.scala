package memo
import scala.collection.mutable.Map

def choose(n: Int, k: Int): Int =
  if k >= n || k <= 0 then 1
  else choose(n - 1, k - 1) + choose(n - 1, k)


def chooseMemo(n: Int, k: Int): Int =
  val cache: Map[(Int, Int), Int] = Map()

  def loop(idxs: (Int, Int)): Int =
    cache.getOrElseUpdate(
      idxs,
      if idxs._2 >= idxs._1 || idxs._2 <= 0 then 1
      else loop((idxs._1 - 1, idxs._2 - 1)) + loop((idxs._1 - 1, idxs._2))
    )

  loop((n, k))


def chooseIter(n: Int, k: Int): Int =
  val cache = Array.ofDim[Int](n + 1, k + 1)

  for n0 <- (0 to n) do
    for k0 <- (0 to math.min(n0, k)) do
      cache(n0)(k0) = 
        if k0 >= n0 || k0 <= 0 then 1
        else cache(n0 - 1)(k0 - 1) + cache(n0 - 1)(k0)

  if k >= n || k <= 0 then 1
  else cache(n)(k)


def chooseIterFinal(n: Int, k: Int): Int =
  ???


def chooseIterFinalOpt(n: Int, k: Int): Int =
  if k <= 0 || k >= n then 1
  else
    var col = Array.fill(math.min(n, k) + 1)(1)
    for
      nn <- 2 to n
      kk <- math.min(k, nn - 1) until 0 by -1
    do
      col(kk) = col(kk - 1) + col(kk)
    col(k)
    

def chooseIterFinalGC(n: Int, k: Int): Int =
  if k <= 0 || k >= n then 1
  else
    val arrLen = n - k + 1
    val diag = Array.fill(arrLen)(1)
    for
      _ <- 1 to k
      i <- 1 until arrLen
    do
      diag(i) = diag(i) + diag(i - 1)
    diag(n - k)
