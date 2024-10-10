package specs

/** Construct all subsets of `s` of size `k` (INCORRECT!) */
def badCombinations[T](set: Set[T], k: Int): Set[Set[T]] = {
  if k == 0 then Set(Set())
  if set.isEmpty then Set()
  else
    for
      item <- set
      rest = set - item
      subset <- Iterable.concat(
        for s <- badCombinations(rest, k - 1) yield rest + item,
        for s <- badCombinations(rest, k) yield rest
      )
    yield subset
}
