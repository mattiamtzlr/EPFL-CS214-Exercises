package comprehensions

object Glob:
  def glob(pattern: List[Char], input: List[Char]): Boolean = (pattern, input) match
    case (Nil, Nil)            => true
    case (Nil, _)              => false
    case ('?' :: pt, _ :: it)  => glob(pt, it)
    case ('*' :: pt, Nil)      => glob(pt, input)
    case ('*' :: pt, _ :: it)  => glob(pt, input) || glob(pattern, it)
    case (p :: pt, Nil)        => false
    case (p :: pt, i :: it)    => p == i && glob(pt, it)

  // overloaded for convenience
  def glob(pattern: String, input: String): Boolean = glob(pattern.toList, input.toList)