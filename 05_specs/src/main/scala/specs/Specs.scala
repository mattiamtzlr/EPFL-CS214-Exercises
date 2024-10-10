package specs


object GradeComputations:
  def OverallGrade(labScore: Double, midtermScore: Double, finalScore: Double): Double =
    ???









def filterWithIncompleteSpec[T](l: List[T], p: T => Boolean) = {
  l.filter(p)
} ensuring (res => res.forall(p))

def mapWithIncompleteSpec[T, Q](ts: List[T])(f: T => Q) = {
  ts.map(f)
} ensuring (qs =>
  qs.length == ts.length &&
    qs.forall(q => ts.exists(t => f(t) == q))
)

def flattenWithIncompleteSpec[T](tss: List[List[T]]) = {
  tss.flatten
} ensuring (ts =>
  ts.length == tss.map(_.length).sum &&
    tss.forall(ts.containsSlice(_))
)
