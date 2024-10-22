package comprehensions

import java.util.Locale

object DesLettres:
  def scramble(word: String): String =
    word.toUpperCase().sorted

  def scrambleList(allWords: Set[String]): Map[String, Set[String]] =
    allWords.groupBy(scramble)

  def exactWord(allWords: Set[String], letters: String): Set[String] =
    scrambleList(allWords).getOrElse(scramble(letters), Set.empty)

  def compatible(small: String, large: String): Boolean =
    (small, large) match
      case ("", _) => true
      case (_, "") => false
      case _       =>
        if small.head == large.head then compatible(small.tail, large.tail)
        else if small.head > large.head then compatible(small, large.tail)
        else false

  def longestWord(allWords: Set[String], letters: String): Set[String] =
    val haystack = scramble(letters)
    val words =
      for 
        (needle, words) <- scrambleList(allWords)
        if compatible(needle, haystack)
      yield words
    
    words.maxByOption(_.head.length()).getOrElse(Set.empty)

object DesChiffres:
  trait Expr:
    val value: Option[Int]

  case class Num(n: Int) extends Expr:
    val value = Some(n)
    override def toString(): String = f"$n" // Print as number

  abstract class Binop extends Expr:
    val e1, e2: Expr // Subexpressions
    def op(n1: Int, n2: Int): Option[Int] // How to evaluate this operator

    val opStr: String // How to print this operator
    override def toString(): String = f"($e1 $opStr $e2)"

    val value: Option[Int] =
      for
        n1 <- e1.value
        n2 <- e2.value
        r <- op(n1, n2)
      yield r

  case class Add(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      Some(n1 + n2)
    val opStr = "+"

  case class Sub(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      if n1 < n2 then None else Some(n1 - n2)
    val opStr = "-"

  case class Mul(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      Some(n1 * n2)
    val opStr = "*"

  case class Div(e1: Expr, e2: Expr) extends Binop:
    def op(n1: Int, n2: Int) =
      if n2 != 0 && n1 % n2 == 0 then Some(n1 / n2) else None
    val opStr = "/"

  def partitions[A](l: List[A]): List[(List[A], List[A])] = l match
    case Nil    => List((Nil, Nil))
    case h :: t =>
      for 
        // for every partition in tail...
        (l1, l2) <- partitions(t)
        // ...generate the two new partitions, by once appending to l1 and once to l2
        partition <- List((h :: l1, l2), (l1, h :: l2))
      yield partition

  def allTrees(ints: List[Int]): List[Expr] = ints match
    case Nil     => Nil
    // if there is only one number, the only possibility is a number
    case List(n) => List(Num(n)) 
    case _       =>
      val trees = for
        (ints1, ints2) <- partitions(ints)       // for all partitions
        if !(ints1.isEmpty || ints2.isEmpty)  // if they aren't empty
        tree1 <- allTrees(ints1)              // recursively construct the left tree
        tree2 <- allTrees(ints2)              // and the right tree
        op    <- List(Add(_, _), Sub(_, _), Mul(_, _), Div(_, _))
        res = op(tree1, tree2)                // construct a result (Option) for every operator
        if !res.value.isEmpty                 // if not None
      yield res

      trees.groupBy(_.value).map(_._2.head).toList

  def leCompteEstBon(ints: List[Int], target: Int): Option[Expr] =
    allTrees(ints).find(_.value == Some(target))