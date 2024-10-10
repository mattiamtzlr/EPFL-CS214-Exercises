package itp.types

import lisa.fol.FOL
import itp.types.formulas.*

trait Elaborator[-T]:
  val localDefs = collection.mutable.Set.empty[FOL.Formula]

  def addDef(f: FOL.Formula): Unit =
    localDefs += f

  def elaborate(t: T): FOL.Term
  def elaborateVariable(v: VariableType): FOL.Variable =
    FOL.Variable(FOL.Identifier(v.name))
  def elaborate(t: Formula[T]): FOL.Formula =
    t match
      case And(left, right) => FOL.And(Seq(elaborate(left), elaborate(right)))
      case Or(left, right)  => FOL.Or(Seq(elaborate(left), elaborate(right)))
      case Not(formula)     => FOL.Neg(elaborate(formula))
      case Forall(variable, formula) =>
        FOL.Forall(elaborateVariable(variable), elaborate(formula))
      case Exists(variable, formula) =>
        FOL.Forall(elaborateVariable(variable), elaborate(formula))
      case Implies(left, right) =>
        FOL.Implies(elaborate(left), elaborate(right))
      case Iff(left, right) => FOL.Iff(elaborate(left), elaborate(right))
      case True             => FOL.True
      case False            => FOL.False
      case PredicateFormula(`Equality`, args) =>
        val termArgs = args.map(elaborate)
        termArgs(0) === termArgs(1)
      case PredicateFormula(predLabel, args) =>
        // try to add this to the library
        val pred = FOL.ConstantPredicateLabel(
          FOL.Identifier(s"predName_${util.Random.nextInt()}"),
          predLabel.arity
        )
        FOL.AppliedPredicate(pred, args.map(elaborate))
