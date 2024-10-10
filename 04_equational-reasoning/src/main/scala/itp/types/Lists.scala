package itp.types
package trees

import itp.types.formulas.*
import lisa.fol.FOL
import lisa.maths.settheory.{SetTheory as ST}

object Lists:

  sealed trait Expr[+A]

  sealed trait T

  // base objects of type T are just variables
  case class Variable[A](name: String) extends T with VariableType
  private case class FunctionApplication(f: T => T, t: T) extends T:
    override def toString(): String = s"$f($t)"

  case class FunctionVariable(name: String)
      extends VariableType
      with Function1[T, T]:
    override def apply(t: T): T = FunctionApplication(this, t)
    override def toString(): String = name

  sealed trait List[+A] extends Expr[A]
  case object Nil extends List[Nothing]
  case class Cons[A](head: A, tail: List[A]) extends List[A]:
    override def toString(): String = s"($head :: $tail)"

  // proof specific objects
  case class ListVariable[A](name: String) extends List[A] with VariableType

  case class ListFunctionVariable(name: String)
      extends VariableType
      with Function1[T, List[T]]:
    override def apply(t: T): List[T] = ListFunctionApplication(this, t)
    override def toString(): String = name

  private case class ListFunctionApplication(f: T => List[T], t: T)
      extends List[T]:
    override def toString(): String =
      f match
        case f: ListFunctionVariable => s"$f($t)"
        case _ =>
          val x = Variable[T]("x")
          s"($x => ${f(x)})($t)"

  private case class AppendList[A](left: List[A], right: List[A])
      extends List[A]:
    override def toString(): String = s"($left ++ $right)"

  private case class ReverseList[A](l: List[A]) extends List[A]:
    override def toString(): String = s"$l.reverse"

  private case class MapList[A](l: List[A], f: A => A) extends List[A]:
    override def toString(): String = s"$l.map($f)"

  private case class LiftedList[A](l: List[A], f: A => List[A])
      extends List[List[A]]:
    override def toString(): String = s"$l.map($f)"

  private case class FlattenList[A](l: List[List[A]]) extends List[A]:
    override def toString(): String = s"$l.flatten"

  private case class FlatMapList[A](l: List[A], f: A => List[A])
      extends List[A]:
    override def toString(): String = s"$l.flatMap($f)"

  extension [A](t: A) infix def ::(l: List[A]): List[A] = Cons(t, l)

  final val single = ListFunctionVariable("single")

  trait Lambda[A]:
    def definition: ProofSystem.Axiom[A]

  case class TLambda(f: T => T) extends Function1[T, T] with Lambda[T]:
    override def apply(a: T): T = FunctionApplication(this, a)
    private val x = Variable[T]("x")
    override def toString(): String = s"($x => ${f(x)})"
    val name =
      val functionName = "func" ++ (f(x)).hashCode().toString
      FunctionVariable(functionName)
    val definition = ProofSystem.Axiom(PredicateFormula(Equality, Seq(name(x), f(x))))

  case class LLambda(f: T => List[T]) extends Function1[T, List[T]] with Lambda[List[T]]:
    override def apply(a: T): List[T] = ListFunctionApplication(this, a)
    private val x = Variable[T]("x")
    override def toString(): String = s"($x => ${f(x)})"
    val name =
      val functionName = "func" ++ (f(x)).hashCode().toString
      ListFunctionVariable(functionName)
    val definition = ProofSystem.Axiom(name(x) === f(x))

  object Lambda:
    def apply(f: T => T) = TLambda(f)
    @annotation.targetName("listApply")
    def apply(f: T => List[T]) = LLambda(f)

  extension [A](l: List[A])
    infix def ++(l2: List[A]): List[A] = AppendList(l, l2)
    def reverse: List[A] = ReverseList(l)
    infix def ===(l2: List[A]): PredicateFormula[List[A]] =
      PredicateFormula(Equality, Seq(l, l2))
    def map(f: A => A): List[A] = MapList(l, f)
    @annotation.targetName("mapList")
    def map(f: A => List[A]): List[List[A]] = LiftedList(l, f)
    def flatMap(f: A => List[A]): List[A] = FlatMapList(l, f)

  extension [A](l: List[List[A]])
    def flatten: List[A] = FlattenList(l)

  type ListExpr = List[T] | List[List[T]]

  object ListElaborator extends Elaborator[ListExpr]:
    private val nil = ST.pair(ST.emptySet, ST.emptySet)
    private val cons = FOL.SchematicFunctionLabel(FOL.Identifier("cons"), 2)
    private val append = FOL.SchematicFunctionLabel(FOL.Identifier("append"), 2)
    private val reverse =
      FOL.SchematicFunctionLabel(FOL.Identifier("reverse"), 1)
    private val map = FOL.SchematicFunctionLabel(FOL.Identifier("map"), 2)
    private val flatmap = FOL.SchematicFunctionLabel(FOL.Identifier("map"), 2)
    private val flatten =
      FOL.SchematicFunctionLabel(FOL.Identifier("flatten"), 1)

    private def testVar = Variable[T]("qIFdmSoufoSsisEW") // random string

    private def elaborateFunction(f: T => T): FOL.Term =
      f match
        case f: FunctionVariable => FOL.Variable(FOL.Identifier(f.name))
        case f: TLambda          => elaborateFunction(f.name)
        case _ =>
          val evaluation = f(testVar)
          val functionName = "func" ++ evaluation.hashCode().toString
          val function = FOL.Variable(FOL.Identifier(functionName))
          val elaboratedEval = elaborateBase(evaluation)
          val funcDef = ST.app(function, elaborateBase(testVar)) === elaboratedEval
          addDef(funcDef)
          function

    private def elaborateListFunction(f: T => List[T]): FOL.Term =
      f match
        case f: ListFunctionVariable => FOL.Variable(FOL.Identifier(f.name))
        case f: LLambda              => elaborateListFunction(f.name)
        case _ =>
          val evaluation = f(testVar)
          val functionName = "func" ++ evaluation.hashCode().toString
          val function = FOL.Variable(FOL.Identifier(functionName))
          val elaboratedEval = elaborateList(evaluation)
          val funcDef = ST.app(function, elaborateBase(testVar)) === elaboratedEval
          addDef(funcDef)
          function

    private def elaborateBase(t: T): FOL.Term =
      t match
        case Variable(name) => FOL.Variable(FOL.Identifier(name))
        case FunctionApplication(f, t) =>
          ST.app(elaborateFunction(f), elaborateBase(t))

    private def elaborateListList(l: List[List[T]]): FOL.Term =
      l match
        case Nil                => nil
        case ListVariable(name) => FOL.Variable(FOL.Identifier(name))
        case Cons(head, tail) =>
          cons(elaborateList(head), elaborateListList(tail))
        case AppendList(left, right) =>
          append(elaborateListList(left), elaborateListList(right))
        case ReverseList(l)   => reverse(elaborateListList(l))
        case LiftedList(l, f) => map(elaborateList(l), elaborateListFunction(f.asInstanceOf[T => List[T]]))
        case MapList(l, f) =>
          throw UnsupportedOperationException(
            "Mapping nested lists not supported in list proofs."
          )
        case FlattenList(l) =>
          throw UnsupportedOperationException(
            "Flattening deeply-nested lists not supported in list proofs."
          )
        case FlatMapList(l, f) =>
          throw UnsupportedOperationException(
            "Flatmapping nested lists not supported in list proofs."
          )

    private def elaborateList(l: List[T]): FOL.Term =
      l match
        case Nil              => nil
        case Cons(head, tail) => cons(elaborateBase(head), elaborateList(tail))
        case AppendList(left, right) =>
          append(elaborateList(left), elaborateList(right))
        case ReverseList(l)     => reverse(elaborateList(l))
        case ListVariable(name) => FOL.Variable(FOL.Identifier(name))
        case MapList(l, f)      => map(elaborateList(l), elaborateFunction(f.asInstanceOf[T => T]))
        case FlattenList(l)     => flatten(elaborateListList(l))
        case ListFunctionApplication(f, t) =>
          ST.app(elaborateListFunction(f), elaborateBase(t))
        case FlatMapList(l, f) => flatmap(elaborateList(l), elaborateListFunction(f.asInstanceOf[T => List[T]]))

    def elaborate(t: ListExpr): FOL.Term =
      // really stupid workaround for type erasure
      try elaborateList(t.asInstanceOf[List[T]])
      catch case _ => elaborateListList(t.asInstanceOf[List[List[T]]])

  given Elaborator[ListExpr] = ListElaborator
