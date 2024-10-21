package poly

def choose2(chars: Seq[Char]): Seq[String] =
  for 
    c1 <- chars
    c2 <- chars
    if c1 != c2
  yield List(c1, c2).mkString