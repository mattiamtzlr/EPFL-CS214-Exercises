package itp.types.formulas

import scala.collection.mutable.Queue
import itp.types.Elaborator
import lisa.fol.FOL
import java.io.StringWriter
import lisa.utils.LisaException
import scala.io.AnsiColor

private def currentLocation(using
    file: sourcecode.File,
    line: sourcecode.Line
) =
  val justFile = file.value.split("/").last
  s"${AnsiColor.BOLD}$justFile:${line.value}${AnsiColor.RESET}"

class Library extends lisa.Main:
  import library.*

  object PanickingOutputManager extends lisa.prooflib.OutputManager:
    val stringWriter: StringWriter = new StringWriter()
    def finishOutput(exception: Exception): Nothing = throw exception
    override def lisaThrow(le: LisaException): Nothing = throw le

  given lisa.prooflib.OutputManager = PanickingOutputManager

  def checkProof[T](
      statement: FOL.Sequent,
      proof: (lib: lisa.prooflib.Library) => (pr: lib.Proof) => Unit
  ): Boolean =
    try
      Theorem(statement)((pr: library.Proof) ?=> proof(library)(pr))
      true
    catch
      case e: Exception =>
        // throw e
        false

object ProofSystem:
  trait Justification[+T]:
    def statement: Formula[T]

  case class Axiom[T](statement: Formula[T]) extends Justification[T]
  case class Assumption[T](statement: Formula[T]) extends Justification[T]

  class Proof[T] private[ProofSystem] (val elaborator: Elaborator[T]):
    import Proof.*

    private val steps: Queue[ProofStep[T]] = Queue.empty
    var state: State[T] = Initial

    private var assumptions: Set[Formula[T]] = Set.empty

    val library: Library = Library()

    def addAssumptions(assumptions: Seq[Formula[T]]): Unit =
      this.assumptions = this.assumptions ++ assumptions

    def hasAssumption(formula: Formula[T]): Boolean =
      assumptions.contains(formula)

    def push(step: ProofStep[T]): Unit =
      steps.enqueue(step)

    def nextStep: ProofStep[T] = steps.dequeue()

    def axiomToFormula[T1 <: T](axiom: Justification[T1]): FOL.Formula =
      def instantiate(f: Formula[T1]): Formula[T1] =
        f match
          case Forall(variable, formula) =>
            instantiate(formula)
          case _ => f

      elaborator.elaborate(instantiate(axiom.statement))

    def validateNext(step: ProofStep[T]): ValidationResult[T] =
      push(step)
      state match
        case Initial =>
          step match
            case ConsiderFormula(formula) =>
              state = ConsideredFormula(formula, formula)
              Valid
            case ConsiderTerm(term) =>
              state = ConsideredTerm(term, term)
              Valid
            case _ =>
              state = Tainted
              Invalid(
                "Cannot consider an equality or equivalence before a formula or term has eben considered with `consider`"
              )
        case ConsideredFormula(formula, lastProven) =>
          step match
            case ConsiderFormula(expression) =>
              state = Tainted
              Invalid(
                "Cannot consider a formula after a formula has been considered"
              )
            case ConsiderTerm(expression) =>
              state = Tainted
              Invalid(
                "Cannot consider a term after a formula has been considered"
              )
            case Equivalent(expression, justification) =>
              val just = axiomToFormula(justification)
              val prev = elaborator.elaborate(lastProven <=> lastProven)
              val premises = assumptions.map(elaborator.elaborate) + just + prev

              val p: (lib: lisa.prooflib.Library) => (proof: lib.Proof) => Unit =
                (lib: lisa.prooflib.Library) =>
                  (proof: lib.Proof) =>
                    val rules =
                      justification match
                        case Assumption(_) => Seq(just)
                        case _             => Seq(lib.Axiom(just))
                    lib
                      .have(using proof)(
                        FOL.Sequent(
                          premises,
                          Set(prev)
                        )
                      )
                      .by(using proof)(
                        lisa.prooflib.SimpleDeducedSteps.Restate
                      )
                    lib
                      .have(using proof)(
                        FOL.Sequent(
                          premises,
                          Set(
                            FOL.Iff(
                              elaborator.elaborate(expression),
                              elaborator.elaborate(lastProven)
                            )
                          )
                        )
                      )
                      .by(using proof)(
                        lisa.automation.Substitution.ApplyRules(using
                          lib,
                          proof
                        )(rules*)(lib.lastStep(using proof))
                      )
              val correct = library.checkProof(
                FOL.Sequent(
                  premises,
                  Set(
                    FOL.Iff(
                      elaborator.elaborate(expression),
                      elaborator.elaborate(lastProven)
                    )
                  )
                ),
                p
              )
              if correct then
                state = ConsideredFormula(formula, expression)
                Valid
              else
                state = Tainted
                InvalidEquivalence(lastProven, expression, justification)
            case Equal(expression, justification) =>
              state = Tainted
              Invalid(
                "A formula has been considered, cannot show a formula equal to a term"
              )

        case ConsideredTerm(term, lastProven) =>
          step match
            case ConsiderFormula(expression) =>
              state = Tainted
              Invalid(
                "Cannot consider a formula after a term has been considered"
              )
            case ConsiderTerm(expression) =>
              state = Tainted
              Invalid("Cannot consider a term after a term has been considered")
            case Equivalent(expression, justification) =>
              state = Tainted
              Invalid(
                "A term has been considered, cannot show a formula equivalent to a term"
              )
            case Equal(expression, justification) =>
              val just = axiomToFormula(justification)
              val prev = elaborator.elaborate(
                PredicateFormula(Equality, Seq(lastProven, lastProven))
              )
              val premises = assumptions.map(elaborator.elaborate) + prev
              val p: (lib: lisa.prooflib.Library) => (proof: lib.Proof) => Unit =
                (lib: lisa.prooflib.Library) =>
                  (proof: lib.Proof) =>
                    val localDefs = elaborator.localDefs.map(lib.Axiom(_)).toSeq
                    val rules =
                      justification match
                        case Assumption(_) => localDefs :+ just
                        case _             => localDefs :+ lib.Axiom(just)
                    lib
                      .have(using proof)(
                        FOL.Sequent(
                          premises,
                          Set(prev)
                        )
                      )
                      .by(using proof)(
                        lisa.prooflib.BasicStepTactic.RewriteTrue
                      )
                    lib
                      .have(using proof)(
                        FOL.Sequent(
                          premises,
                          Set(
                            elaborator.elaborate(
                              PredicateFormula(
                                Equality,
                                Seq(expression, lastProven)
                              )
                            )
                          )
                        )
                      )
                      .by(using proof)(
                        lisa.automation.Substitution.ApplyRules(using
                          lib,
                          proof
                        )(rules*)(lib.lastStep(using proof))
                      )
              val correct = library.checkProof(
                FOL.Sequent(
                  premises,
                  Set(
                    FOL.equality(
                      elaborator.elaborate(expression),
                      elaborator.elaborate(lastProven)
                    )
                  )
                ),
                p
              )
              if correct then
                state = ConsideredTerm(term, expression)
                Valid
              else
                state = Tainted
                InvalidEquality(lastProven, expression, justification)
        case Tainted =>
          Invalid("Cannot perform a step on a tainted proof")
        case Finished =>
          Invalid(
            "Proof is finished, an equivalence was proven. Cannot perform a step."
          )

  object Proof:
    sealed trait State[+T]
    case object Initial extends State[Nothing]
    case class ConsideredFormula[T](formula: Formula[T], lastProven: Formula[T])
        extends State[T]
    case class ConsideredTerm[T](term: T, lastProven: T) extends State[T]
    case object Tainted extends State[Nothing]
    case object Finished extends State[Nothing]

    sealed abstract class ValidationResult[+T]:
      def isValid: Boolean = false
      def message: String

    case object Valid extends ValidationResult[Nothing]:
      override def isValid: Boolean = true
      def message: String = "Valid"

    case class Invalid(msg: String) extends ValidationResult:
      def message: String = msg

    case class InvalidEquality[T](
        left: T,
        right: T,
        justification: Justification[T]
    ) extends ValidationResult:
      def message: String =
        s"\n\tLHS: ${AnsiColor.BOLD}$left${AnsiColor.RESET} \n\tRHS: ${AnsiColor.BOLD}$right${AnsiColor.RESET}\nCannot be justified with the statement: ${AnsiColor.BOLD}${justification.statement}${AnsiColor.RESET}"

    case class InvalidEquivalence[T](
        left: Formula[T],
        right: Formula[T],
        justification: Justification[T]
    ) extends ValidationResult:
      def message: String =
        s"\n\tLHS: $left \n\tRHS: $right\nCannot be justified with the statement: ${justification.statement}"

  sealed trait ProofStep[+T]
  case class ConsiderFormula[T](expression: Formula[T]) extends ProofStep[T]
  case class ConsiderTerm[T](expression: T) extends ProofStep[T]
  case class Equivalent[T](
      expression: Formula[T],
      justification: Justification[T]
  ) extends ProofStep[T]
  case class Equal[T](expression: T, justification: Justification[T])
      extends ProofStep[T]

  object Syntax:

    def consider[T](using
        p: Proof[T]
    )(using
        file: sourcecode.File,
        line: sourcecode.Line
    )(expression: Formula[T]): Unit =
      val step = ConsiderFormula[T](expression)
      val result = p.validateNext(step)
      require(
        result.isValid,
        s"\n${currentLocation(using file, line)} : proof step failed: ${result.message}"
      )

    def consider[T](using
        p: Proof[T]
    )(using file: sourcecode.File, line: sourcecode.Line)(expression: T): Unit =
      val step = ConsiderTerm[T](expression)
      val result = p.validateNext(step)
      require(
        result.isValid,
        s"\n${currentLocation(using file, line)} : proof step failed: ${result.message}"
      )

    def andThen[T](using
        p: Proof[T]
    )(using
        file: sourcecode.File,
        line: sourcecode.Line
    )(expression: T)(justification: Justification[T]) =
      val step = Equal(expression, justification)
      val result = p.validateNext(step)
      require(
        result.isValid,
        s"\n${currentLocation(using file, line)} : proof step failed: ${result.message}"
      )

    def andThen[T](using p: Proof[T])(using
        file: sourcecode.File,
        line: sourcecode.Line
    )(expression: Formula[T])(
        justification: Justification[T]
    ) =
      val step = Equivalent(expression, justification)
      val result = p.validateNext(step)
      require(
        result.isValid,
        s"\n${currentLocation(using file, line)} : proof step failed: ${result.message}"
      )

    def andThen[T](using
        p: Proof[T]
    )(using file: sourcecode.File, line: sourcecode.Line)(expression: T)(
        justification: Formula[T]
    ) =
      // is this justification a known assumption?
      require(
        p.hasAssumption(justification),
        s"At ${currentLocation(using file, line)}, cannot use the formula $justification as a justification, it has not been assumed."
      )
      val step = Equal(expression, Assumption(justification))
      val result = p.validateNext(step)
      require(
        result.isValid,
        s"\n${currentLocation(using file, line)} : proof step failed: ${result.message}"
      )

    def andThen[T](using p: Proof[T])(using
        file: sourcecode.File,
        line: sourcecode.Line
    )(expression: Formula[T])(
        justification: Formula[T]
    ) =
      require(
        p.hasAssumption(justification),
        s"At ${currentLocation(using file, line)}, cannot use the formula $justification as a justification, it has not been assumed."
      )
      val step = Equivalent(expression, Assumption(justification))
      val result = p.validateNext(step)
      require(
        result.isValid,
        s"\n${currentLocation(using file, line)} : proof step failed: ${result.message}"
      )

    extension [T, T1 >: T](f: Iff[T])
      infix def ==<(using p: Proof[T1])(using
          file: sourcecode.File,
          line: sourcecode.Line
      )(justification: Justification[T1]) =
        f match
          case Iff(left, right) =>
            // has the proof been initialized?
            if p.state == Proof.Initial then
              consider(using p)(using file, line)(left)
            andThen[T1](using p)(using file, line)(right)(justification)
            right

    extension [T, T1 >: T](f: PredicateFormula[T])
      infix def ==<(using p: Proof[T1])(using
          file: sourcecode.File,
          line: sourcecode.Line
      )(justification: Justification[T1]) =
        f match
          case PredicateFormula(Equality, Seq(left, right)) =>
            // has the proof been initialized?
            if p.state == Proof.Initial then
              consider(using p)(using file, line)(left)
            andThen[T1](using p)(using file, line)(right)(justification)
            right
          case _ =>
            throw IllegalArgumentException(
              "Proof step can only be applied on an equality (===) or an equivalence (<=>)"
            )

    extension [T, T1 >: T](f: Iff[T])
      infix def ==<(using
          file: sourcecode.File,
          line: sourcecode.Line
      )(using p: Proof[T1])(justification: Formula[T1]) =
        f match
          case Iff(left, right) =>
            // has the proof been initialized?
            if p.state == Proof.Initial then consider(using p)(left)
            andThen[T1](using p)(using file, line)(right)(justification)
            right

    extension [T, T1 >: T](f: PredicateFormula[T])
      infix def ==<(using
          file: sourcecode.File,
          line: sourcecode.Line
      )(using p: Proof[T1])(justification: Formula[T1]) =
        f match
          case PredicateFormula(Equality, Seq(left, right)) =>
            // has the proof been initialized?
            if p.state == Proof.Initial then consider(using p)(left)
            andThen[T1](using p)(using file, line)(right)(justification)
            right
          case _ =>
            throw IllegalArgumentException(
              "Proof step can only be applied on an equality (===) or an equivalence (<=>)"
            )

  export Syntax.*

  class Theorem[T](using elaborator: Elaborator[T])(using
      name: sourcecode.Name,
      file: sourcecode.File,
      line: sourcecode.Line
  )(val statement: Formula[T])(
      computeProof: Proof[T] ?=> Unit
  ) extends Justification[T]:
    var checked: Boolean = false
    val innerProof: Proof[T] = Proof[T](elaborator)
    val (assumptions, statementWithoutAssumptions) =
      Theorem.destructStatement(statement)
    innerProof.addAssumptions(assumptions)
    try
      require(
        {
          computeProof(using innerProof)
          innerProof.state match
            case Proof.ConsideredTerm(term, lastProven) =>
              statementWithoutAssumptions == (PredicateFormula(
                Equality,
                Seq(term, lastProven)
              )) || statementWithoutAssumptions == (PredicateFormula(
                Equality,
                Seq(lastProven, term)
              ))
            case Proof.ConsideredFormula(term, lastProven) =>
              statementWithoutAssumptions == Iff(
                term,
                lastProven
              ) || statementWithoutAssumptions == Iff(lastProven, term)
            case _ => false

        },
        s"Proof at ${currentLocation(using file, line)} does not prove the required statement: $statement.\nThe proof ${
            if innerProof.state == Proof.Tainted then "ran into an error"
            else "is unfinished"
          }."
      )
      println(
        s"Proved Theorem ${name.value}: ${AnsiColor.BOLD}$statement${AnsiColor.RESET}"
      )
      checked = true
    catch
      case scala.util.control.NonFatal(e) => println(e)

    def verify: Boolean =
      assert(checked, "Theorem has not been verified.")
      true

  object Theorem:
    private def destructFormulaConjunction[T](
        formula: Formula[T]
    ): Seq[Formula[T]] =
      formula match
        case And(left, right) =>
          destructFormulaConjunction(left) ++ destructFormulaConjunction(right)
        case _ => Seq(formula)

    private def destructStatement[T](
        statement: Formula[T]
    ): (Seq[Formula[T]], Formula[T]) =
      statement match
        case Implies(left, right) =>
          val (assumptions, statementWithoutAssumptions) = destructStatement(
            right
          )
          (
            destructFormulaConjunction(left) ++ assumptions,
            statementWithoutAssumptions
          )
        case _ => (Seq.empty, statement)
