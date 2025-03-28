package tailRecursion.lists

def foldLeftForeach[B, A](z: B)(op: (B, A) => B)(xs: List[A]): B =
  var res = z
  
  for x <- xs do {
    res = op(res, x)
  }

  res