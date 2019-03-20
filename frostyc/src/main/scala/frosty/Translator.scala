package frosty

import frosty.ir.{Ir, IrValue, liftIr}
import frosty.ast._
import frosty.types._
import frosty.algebra.{Term, Env}
import frosty.bytecode.builtin.BuiltInChannel


private[frosty]
object Translator extends BackendPhase[List[TypedAst], List[Ir]] (
  PhaseName.Translate,
  """Translates typed terms into intermediate representation."""
) {

  def safeCompile(asts: List[TypedAst]): List[Ir] = {
    asts.par.map(translate)(collection.breakOut)
  }

  /** Translates a single typed AST into intermediate representation. */
  def translate(ast: TypedAst): Ir = {
    /* Translated value, together with it's originally inferred type.
     * The type can still be necessary in some cases, for example when
     * compiling `==` or comparison operators: the external type of the
     * whole expression is `Boolean`, so we have to look at the type of the
     * compared values. That's where the second component in this tuple
     * comes in handy.
     */
    type IrMixed = (Either[IrValue, Ir], Type)

    /* Assumes that this is a `IrValue` and extracts it. */
    def asValue(mixed: IrMixed): IrValue = mixed._1.left.get

    /* Assumes that this is an `Ir` and extracts it. */
    def asProc(mixed: IrMixed): Ir = mixed._1.right.get

    /* The more important part of the two-part `fold` of the Term */
    def translateCons(
      tAstCons: TypedAstC[IrMixed, (List[String], IrMixed)]
    ): IrMixed = {
      val (typ, astC) = tAstCons
      val resultTerm = if (typ == ProcessStatement) {
        // This should produce an `Ir` wrapped in a `Right`
        Right(astC match {
          case Parallel(procs) => liftIr(ir.Parallel(procs map asProc))
          case Tell(ch, ms) => liftIr(ir.Tell(asValue(ch), ms map asValue))
          case Receive(chan, (names, body)) => {
            liftIr(ir.Receive(asValue(chan), names, asProc(body)))
          }
          case Ascription(proc, _typeAlreadyChecked) => asProc(proc)
          case New(_typesErased, (names, body)) => {
            liftIr(ir.New(names, asProc(body)))
          }
          case Contract(chan, (names, body)) => {
            liftIr(ir.Contract(asValue(chan), names, asProc(body)))
          }
          case Def(chan, (names, body)) => {
            liftIr(ir.FuncDef(asValue(chan), names, asValue(body)))
          }
          case IfElse(c, t, e) => {
            liftIr(ir.IfElseP(asValue(c), asProc(t), asProc(e)))
          }
          case Unfreeze(p) => liftIr(ir.Unfreeze(asValue(p)))
          case Invocation(runnable, args) => {
            liftIr(ir.Tell(asValue(runnable), args map asValue))
          }
          case sthElse => Typer.blameFor(
            ProcessStatement,
            "Unexpected AST typed as `ProcessStatement`: " + sthElse
          )
        })
      } else {
        // This should produce an `IrValue` wrapped in a `Left`
        Left(astC match {
          case Parallel(procsAndValue) => {
            liftIr(ir.ValueBlock(
              procsAndValue.init.map(asProc),
              asValue(procsAndValue.last)
            ))
          }
          case Invocation(func, args) => 
            liftIr(ir.Invocation(asValue(func), args map asValue))

          case AbsoluteName(p) => {
            BuiltInChannel.tryFrom(p.path) match {
              case None => liftIr(ir.AbsolutePathName(p))
              case Some(bic) => liftIr(ir.BuiltInChannelName(bic))
            }
          }
          case n @ RelativeName(_) => SymbolResolver.blameFor(n)
          case UnitValue => liftIr(ir.UnitValue)
          case B(b) => liftIr(ir.B(b))
          case Not(b) => liftIr(ir.NotB(asValue(b)))
          case And(a, b) => liftIr(ir.AndB(asValue(a), asValue(b)))
          case Or (a, b) => liftIr(ir.OrB(asValue(a), asValue(b)))
          case e @ Eq(a @ (_, aTyp), b @ (_, _same)) => aTyp match {
            case IntType => liftIr(ir.EqI(asValue(a), asValue(b)))
            case StringType => liftIr(ir.EqS(asValue(a), asValue(b)))
            case UnitType => liftIr(ir.EqU(asValue(a), asValue(b)))
            case BooleanType => liftIr(ir.EqB(asValue(a), asValue(b)))
            case weirdType => Typer.blameFor(
              weirdType,
              "Equality supported only for primitive built-in types, but " +
              "found expression of shape `a == b` where the type is not " + 
              "primitive: \n" + e
            )
          }
          case I(i) => liftIr(ir.I(i.toInt /* If this fails: Typer's fault */))
          case ast @ Add(a, b) => typ match {
            case StringType => liftIr(ir.ConcatS(asValue(a), asValue(b)))
            case IntType => liftIr(ir.AddI(asValue(a), asValue(b)))
            case sthElse => Typer.blameFor(
              sthElse,
              "Failed to translate `Add` (+) for ast: " + ast
            )
          }
          // TODO: Everything only for `Int`, should at least check types!!!
          case Sub(a, b) => liftIr(ir.SubI(asValue(a), asValue(b)))
          case Mul(a, b) => liftIr(ir.MulI(asValue(a), asValue(b)))
          case Div(a, b) => liftIr(ir.DivI(asValue(a), asValue(b)))
          case Rem(a, b) => liftIr(ir.RemI(asValue(a), asValue(b)))
          case Neg(a) => liftIr(ir.NegI(asValue(a)))
          case S(s) => liftIr(ir.S(s))
          case Freeze(p) => liftIr(ir.Freeze(asProc(p)))
          case IfElse(cond, t, e) =>
            liftIr(ir.IfElseE(asValue(cond), asValue(t), asValue(e)))
          case New(_types, (names, body)) => 
            liftIr(ir.NewExpr(names, asValue(body)))
          case Await(ch) => liftIr(ir.Await(asValue(ch)))
          case Receive(c, (names, body)) => {
            liftIr(ir.ReceiveExpr(asValue(c), names, asValue(body)))
          }
          case sthElse => throw new AssertionError(
            s"Unexpected AST typed as expression ($typ):\n" +
            sthElse
          )
        })
      }

      (resultTerm, typ)
    }

    /* The simpler part of the `fold` */
    def translateVar(name: String, typ: Type): IrMixed = {
      (Left(liftIr(ir.LocalVar(name))), typ)
    }

    asProc(Term.fold(ast)(translateCons, translateVar))
  }
}