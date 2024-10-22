package specs

def binomCoeff(n: Int, k : Int): BigInt = 
  def factorial(n: Int): BigInt =
    (1 to n).map(BigInt(_)).foldLeft(BigInt(1))(_ * _)

  if n == 0 && k != 0 then 0
  else factorial(n) / (factorial(k) * factorial(n - k))

/** Construct all subsets of `s` of size `k` (INCORRECT!) */
def combinations[T](set: Set[T], k: Int): Set[Set[T]] = {
  require(k >= 0)
  if k == 0 then Set(Set.empty)
  else if set.isEmpty then Set.empty
  else
    val item = set.head
    val rest = set - item
    (for
      subset <- Iterable.concat(
        for s <- combinations(rest, k - 1) yield s + item,
        for s <- combinations(rest, k) yield s 
      )
    yield subset).toSet
} ensuring (
  res => 
    res.forall(s => s.size == k && s.subsetOf(set))
    && BigInt(res.size) == binomCoeff(set.size, k)
)