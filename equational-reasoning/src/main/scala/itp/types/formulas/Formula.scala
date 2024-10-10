package itp.types
package formulas

sealed trait Formula[+T]
case class And[T](left: Formula[T], right: Formula[T]) extends Formula[T]:
  override def toString(): String = s"($left /\\ $right)"
case class Or[T](left: Formula[T], right: Formula[T]) extends Formula[T]:
  override def toString(): String = s"($left \\/ $right)"
case class Not[T](formula: Formula[T]) extends Formula[T]:
  override def toString(): String = s"¬($formula)"
case class Forall[T](variable: VariableType, formula: Formula[T])
    extends Formula[T]:
  override def toString(): String = s"∀$variable. $formula"
case class Exists[T](variable: VariableType, formula: Formula[T])
    extends Formula[T]:
  override def toString(): String = s"∃$variable. $formula"
case class Implies[T](left: Formula[T], right: Formula[T]) extends Formula[T]:
  override def toString(): String = s"($left ==> $right)"
case class Iff[T](left: Formula[T], right: Formula[T]) extends Formula[T]:
  override def toString(): String = s"($left <=> $right)"

case object True extends Formula[Nothing]
case object False extends Formula[Nothing]

trait VariableType:
  val name: String
  override def toString(): String = s"$name"

extension [T](f: Formula[T])
  infix def `/\\`(g: Formula[T]): Formula[T] = And(f, g)
  infix def `\\/`(g: Formula[T]): Formula[T] = Or(f, g)
  def unary_! : Formula[T] = Not(f)
  infix def ==>(g: Formula[T]): Formula[T] = Implies(f, g)
  infix def <=>(g: Formula[T]): Formula[T] = Iff(f, g)

def forall[T](x: VariableType, f: Formula[T]): Formula[T] = Forall(x, f)
def exists[T](x: VariableType, f: Formula[T]): Formula[T] = Exists(x, f)

case class PredicateFormula[T](label: PredicateLabel, args: Seq[T])
    extends Formula[T]:
  override def toString(): String =
    if label.infix then
      assert(label.arity == 2)
      s"(${args(0)} ${label.name} ${args(1)})"
    else s"${label.name}(${args.mkString(", ")})"

case class PredicateLabel(name: String, arity: Int, infix: Boolean = false)

val Equality = PredicateLabel("===", 2, true)
