package comprehensions

def isSafe(col: Int, queens: List[Int]): Boolean =
  val row = queens.length
  queens.zipWithIndex.forall {
    case (c, r) => 
      c != col                          // check for same row
      && (row - r).abs != (col - c).abs // check for same diagonal
  }

def queens(n: Int) =
  def placeQueens(k: Int): Set[List[Int]] =
    if k == 0 then Set(List())
    else
      for
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if isSafe(col, queens)
      yield col :: queens
  placeQueens(n)
