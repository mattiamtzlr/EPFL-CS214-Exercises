package pbt

def mSort(ls: List[Int]): List[Int] =
  ls match
    case Nil            => Nil
    case a :: Nil       => a :: Nil
    case a :: b :: tail =>
      val (ll, lr) = split(ls)
      merge(mSort(ll), mSort(lr))

    /** Check if a given list is sorted */
def isSortedAscending(ls: List[Int]): Boolean =
  ls == ls.sortWith(_ < _)

/** Given two sorted lists, merge them into a new sorted list */
def merge(ll: List[Int], lr: List[Int]): List[Int] = {
  require(isSortedAscending(ll) && isSortedAscending(lr))

  (ll, lr) match
    case (Nil, _) => lr
    case (_, Nil) => ll
    case (lh :: lt, rh :: rt) =>
      if lh < rh then lh :: merge(lt, lr)
      else rh :: merge(ll, rt)
}.ensuring { l =>
  l == (ll ++ lr).sortWith(_ < _)
}

/** Split a list into two lists containing the first and second halves
  * respectively
  */
def split[A](ls: List[A]): (List[A], List[A]) = {
  val (ll, lr) = ls.splitAt(ls.length / 2)
  (ll, lr)

}.ensuring { case (ll, lr) =>
  ls == ll ++ lr && ll.length == ls.length / 2
}
