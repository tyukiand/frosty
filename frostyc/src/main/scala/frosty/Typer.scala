package frosty

import cats.Applicative
import cats.data.Chain
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import collection.mutable.{HashMap, ListBuffer}
import frosty.algebra.{Term, Env}
import frosty.ast._
import frosty.bytecode.builtin.BuiltInChannel
import frosty.namespace.AbsolutePath
import frosty.types._
import frosty.types.Type.isPrimitive
import scala.collection.breakOut
import scala.language.higherKinds
import scala.util.Either

object Typer extends Phase[
  (List[SymbolDeclaration], List[Ast]),
  List[TypedAst]
](
  PhaseName.Type,
  "Checks / infers types for all expressions."
){

  /** Piece of symbol table that maps names of built-in channels to their
    * types. The types are specified in snippets Frosty-syntax inside the
    * external Rust file that contains the macros with the opcode table.
    * The information from this table is extracted during code generation.
    */
  private val PreambleSymbolTable = {
    BuiltInChannel
      .all
      .map(c => AbsolutePath(c.path) -> c.typ)
      .toMap
  }

  private def cCompErr(pos: Position, msg: String) =
    Chain(CompilationError(pos, msg))

  def apply(input: (List[SymbolDeclaration], List[Ast]))
  : CompilationErrorsOr[List[TypedAst]] = {
    val (symbolDeclarations, asts) = input
    val symbolTable: Map[AbsolutePath, Type] =
      (
        symbolDeclarations.map(d => (d.name, d.typ))(breakOut)
        : Map[AbsolutePath, Type]
      ) ++
      PreambleSymbolTable
    
    asts
    .par
    .map(typeAst(_, symbolTable))
    .toList
    .sequence
    .toPhaseErrors
  }

  def typeAst(ast: Ast, symbolTable: Map[AbsolutePath, Type])
  : ValidationErrorsOr[TypedAst] = {
    val addTypes: Term.ApplicativeEvalAlgebra[
      PositionedAstC,
      String,
      ValidationErrorsOr,
      TypedAst
    ] = { case (pos, ast) => ast match {

      case Parallel(procs) => {
        if (procs.isEmpty) {
          typedAst(Parallel(Nil), ProcessStatement).pure[ValidationErrorsOr]
        } else {
          procs
          .sequence
          .ensure(cCompErr(pos,
            "In parallel compositions, all elements " +
            "except the last one must be `Proc`s."
          )) {
            ps => {
              ps.isEmpty || 
              ps.init.forall(proc => typeOf(proc) == ProcessStatement) 
            }
          }
          .map(procs => typedAst(Parallel(procs), typeOf(procs.last)))
        }
      }

      case Tell(chan, messages) => {
        (chan, messages.sequence)
        .tupled
        .andThen { case (c, ms) =>
          typeOf(c) match {
            case ChannelType(cts) => {
              val mts = ms map typeOf
              if (mts == cts)
                typedAst(Tell(c, ms), ProcessStatement).pure[ValidationErrorsOr]
              else err(
                pos,
                "Sent messages have unexpected types." +
                "\nExpected: " + cts +
                "\nMessages: " + mts
              )
            }
            case notAChannel => err(
              pos,
              "Cannot send anything to " + notAChannel +
              " Expected a channel."
            )
          }
        }
      }

      case Receive(chan, (names, bodyEvaluator)) => {
        chan
        .andThen { tc => typeOf(tc) match {
          case ChannelType(expArgTypes) => {
            typeInferBinder(
              names,
              expArgTypes,
              bodyEvaluator,
              pos
            )
            .map(binder => typedAst(Receive(tc, binder), typeOf(binder._2)))
          }
          case sthElse => err(pos, "Expected channel, got " + sthElse)
        }}
      }

      case Freeze(p) => {
        p.andThen { tp => typeOf(tp) match {
          case ProcessStatement => 
            typedAst(Freeze(tp), FreezeProcessType)
            .pure[ValidationErrorsOr]
          case sthElse => 
            err(pos, "Only process-statements can be frozen; found: " + sthElse)
        }}
      }

      case Unfreeze(p) => {
        p.andThen { tp => typeOf(tp) match {
          case FreezeProcessType => 
            typedAst(Unfreeze(tp), ProcessStatement)
            .pure[ValidationErrorsOr]
          case sthElse => 
            err(pos, "Only frozen processes can be unfrozen; found: " + sthElse)
        }}
      }

      case Invocation(func, args) => {
        (func, args.sequence)
        .tupled
        .andThen { case (tf, tas) => {
          val argumentTypes = tas map typeOf
          typeOf(tf) match {
            case FunctionType(expArgTypes, retType) => {
              if (expArgTypes == argumentTypes) {
                typedAst(Invocation(tf, tas), retType).pure[ValidationErrorsOr]
              } else {
                err(
                  pos,
                  "Type mismatch " +
                  "\nexpected: " + expArgTypes.mkString(", ") +
                  "\nbut got : " + argumentTypes.mkString(", ")
                )
              }
            }
            case RunnableType(expArgTypes) => {
              if (expArgTypes == argumentTypes) {
                typedAst(Invocation(tf, tas), ProcessStatement)
                .pure[ValidationErrorsOr]
              } else {
                err(
                  pos,
                  "Type mismatch " +
                  "\nexpected: " + expArgTypes.mkString(", ") +
                  "\nbut got : " + argumentTypes.mkString(", ")
                )
              }
            }
            case sthElse => err(pos, "Not invocable: " + sthElse)
          }
        }}
      }
      case Ascription(proc, typ) => 
        throw new NotImplementedError("TODO: Ascription typing")

      case n @ AbsoluteName(path) => {
        symbolTable
        .get(path)
        .map(typ => typedAst(n, typ).pure[ValidationErrorsOr])
        .getOrElse(err[TypedAst](pos, "Not found: " + path))
      }

      case relPath @ RelativeName(path) => SymbolResolver.blameFor(relPath)
      case New(types, (names, bodyEvaluator)) => {
        typeInferBinder(names, types, bodyEvaluator, pos)
        .map(binder => typedAst(New(types, binder), typeOf(binder._2))) // TODO: what is this ._2 good for?
      }
      case Contract(proc, (names, bodyEvaluator)) => {
        proc
        .andThen { tp => typeOf(tp) match {
          case RunnableType(expArgTypes) => {
            typecheckBinder(
              names,
              expArgTypes,
              bodyEvaluator,
              ProcessStatement,
              pos
            )
            .map(binder => typedAst(Contract(tp, binder), ProcessStatement))
          }
          case sthElse => err(pos, "Expected contract type, but got " + sthElse)
        }}
      }
      case Def(typedFunc, (names, bodyEvaluator)) => {
        typedFunc
        .andThen { tf => typeOf(tf) match {
          case FunctionType(expArgTypes, retType) => {
            typecheckBinder(names, expArgTypes, bodyEvaluator, retType, pos)
            .map(binder => typedAst(Def(tf, binder), ProcessStatement))
          }
          case sthElse => err(pos, "Expected function type, but got " + sthElse)
        }}
      }
      case Await(ch) => {
        ch.andThen { tch => typeOf(tch) match {
          case ChannelType(List(returnType)) => 
            typedAst(Await(tch), returnType).pure[ValidationErrorsOr]
          case sthElse => err(pos, "Expected `Chan[?]`, but got " + sthElse)
        }}
      }
      case u @ UnitValue => typedAst(u, UnitType).pure[ValidationErrorsOr]
      case b @ B(_) => typedAst(b, BooleanType).pure[ValidationErrorsOr]
      case IfElse(cond, thenProc, elseProc) => {
        (cond, thenProc, elseProc).tupled.andThen { case (tc, tt, te) =>
          val cType = typeOf(tc)
          val tType = typeOf(tt)
          val eType = typeOf(te)
          if (cType == BooleanType) {
            if (tType == eType) {
              typedAst(IfElse(tc, tt, te), tType).pure[ValidationErrorsOr]
            } else {
              err(
                pos,
                s"Types in both branches of an `if-else` must match, but\n" +
                s"${tType} in the `then`-branch is not the same as\n" +
                s"${eType} in the `else`-branch."
              )
            }
          } else {
            err(
              pos,
              s"Condition in `if-else` expected to be of Boolean type, " +
              s"but was ${cType}."
            )
          }
        }
      }
      case Not(b) => ???
      case And(a, b) => {
        (a, b).tupled.andThen { case (ta, tb) => 
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          (aType, bType) match {
            case (BooleanType, BooleanType) => {
              typedAst(And(ta, tb), BooleanType).pure[ValidationErrorsOr]
            }
            case (weirdA, weirdB) => {
              err(pos, s"Boolean `and` not defined for ${weirdA} ${weirdB}")
            }
          }
        }
      }
      case Or (a, b) => ???
      case Eq (a, b) => {
        (a, b).tupled.andThen { case (ta, tb) =>
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          val resultTypeOpt = if (aType == bType) Some(BooleanType) else None
          resultTypeOpt
            .map(typ => typedAst(Eq(ta, tb), typ).pure[ValidationErrorsOr])
            .getOrElse(err(
              pos,
              s"Operation `==` not defined for $aType==$bType"
            ))
        }
      }
      case i @ I(str) => {
        try {
          str.toInt
          typedAst(i, IntType).pure[ValidationErrorsOr]
        } catch {
          case e: NumberFormatException => {
            err[TypedAst](
              pos, 
              "Invalid integer literal " + 
              "(valid range: ${Int.MinValue} - ${Int.MaxValue})"
            )
          }
        }
      }
      case Add(a, b) => {
        (a, b).tupled.andThen { case (ta, tb) =>
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          val resultTypeOpt = (aType, bType) match {
            case (StringType, StringType) => Some(StringType)
            case (IntType, IntType) => Some(IntType)
            case sthElse => None
          }
          resultTypeOpt
          .map(typ => typedAst(Add(ta, tb), typ).pure[ValidationErrorsOr])
          .getOrElse(err(pos, s"Operation `+` not defined for $aType+$bType"))
        }
      }
      case Sub(a, b) => {
        (a, b).tupled.andThen{ case (ta, tb) =>
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          val resultTypeOpt = (aType, bType) match {
            case (IntType, IntType) => Some(IntType)
            // TOEXTEND: this would be the right place to add doubles etc.
            case sthElse => None
          }
          resultTypeOpt
            .map(tp => typedAst(Sub(ta, tb), tp).pure[ValidationErrorsOr])
            .getOrElse(err(
              pos,
              s"Operation `-` not defined for $aType - $bType"
            ))
        }
      }
      case Mul(a, b) => {
        (a, b).tupled.andThen{ case (ta, tb) =>
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          val resultTypeOpt = (aType, bType) match {
            case (IntType, IntType) => Some(IntType)
            // TOEXTEND: this would be the right place to add doubles etc.
            case sthElse => None
          }
          resultTypeOpt
            .map(tp => typedAst(Mul(ta, tb), tp).pure[ValidationErrorsOr])
            .getOrElse(err(
              pos,
              s"Operation `*` not defined for $aType * $bType"
            ))
        }
      }
      case Div(a, b) => {
        (a, b).tupled.andThen{ case (ta, tb) =>
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          val resultTypeOpt = (aType, bType) match {
            case (IntType, IntType) => Some(IntType)
            // TOEXTEND: this would be the right place to add doubles etc.
            case sthElse => None
          }
          resultTypeOpt
            .map(tp => typedAst(Div(ta, tb), tp).pure[ValidationErrorsOr])
            .getOrElse(err(
              pos,
              s"Operation `/` not defined for $aType / $bType"
            ))
        }
      }
      case Rem(a, b) => {
        (a, b).tupled.andThen{ case (ta, tb) =>
          val aType = typeOf(ta)
          val bType = typeOf(tb)
          val resultTypeOpt = (aType, bType) match {
            case (IntType, IntType) => Some(IntType)
            // TOEXTEND: this would be the right place to add doubles etc.
            case sthElse => None
          }
          resultTypeOpt
            .map(tp => typedAst(Rem(ta, tb), tp).pure[ValidationErrorsOr])
            .getOrElse(err(
              pos,
              s"Operation `%` not defined for $aType % $bType"
            ))
        }
      }
      case Neg(a) => {
        a.andThen { ta =>
          val aType = typeOf(ta)
          val resultTypeOpt = aType match {
            case IntType => Some(IntType)
            // TOEXTEND: this would be the right place to add doubles etc.
            case sthElse => None
          }
          resultTypeOpt
            .map(tp => typedAst(Neg(ta), tp).pure[ValidationErrorsOr])
            .getOrElse(err(
              pos,
              s"Operation unary-`-` not defined for -$aType"
            ))
        }
      }
      case s @ S(_) => typedAst(s, StringType).pure[ValidationErrorsOr]
    }}

    val variableNotFoundError = { (pos: Position, name: String) =>
      err[TypedAst](pos, s"Variable `$name` not found in this scope.")
    }

    val typCtx0 = Env.empty[String, TypedAst]
    Term.evalWithNamesA(ast, typCtx0)(addTypes)(variableNotFoundError)
  }

  private def typecheckBinder(
    names: List[String],
    expArgTypes: List[Type],
    bodyEvaluator: List[TypedAst] => ValidationErrorsOr[TypedAst],
    expectedBodyType: Type,
    pos: Position
  ): ValidationErrorsOr[(List[String], TypedAst)] = {
    if (names.size != expArgTypes.size) {
      err(
        pos,
        s"Wrong number of variables," +
        s" expected ${expArgTypes.size}, got ${names.size}" +
        s" expected types: " + expArgTypes.mkString(", ")
      )
    } else {
      val varTypedAsts = for ((n, t) <- names zip expArgTypes) yield {
        typedAst(n, t)
      }
      bodyEvaluator(varTypedAsts)
      .andThen { tb => 
        if (typeOf(tb) == expectedBodyType) {
          (names, tb).pure[ValidationErrorsOr]
        } else {
          err(
            pos,
            "Type mismatch of body under variable binders: " +
            "\nExpected: " + expectedBodyType +
            "\nBut got : " + typeOf(tb)
          )
        }
      }
    }
  }

  // TODO: duplicates code from typeCheck, clean it up (low-priority)
  private def typeInferBinder(
    names: List[String],
    expArgTypes: List[Type],
    bodyEvaluator: List[TypedAst] => ValidationErrorsOr[TypedAst],
    pos: Position
  ): ValidationErrorsOr[(List[String], TypedAst)] = {
    if (names.size != expArgTypes.size) {
      err(
        pos,
        s"Wrong number of variables," +
        s" expected ${expArgTypes.size}, got ${names.size}" +
        s" expected types: " + expArgTypes.mkString(", ")
      )
    } else {
      val varTypedAsts = for ((n, t) <- names zip expArgTypes) yield {
        typedAst(n, t)
      }
      bodyEvaluator(varTypedAsts).map(typedBody => (names, typedBody))
    }
  }

  def blameFor(typ: Type, msg: String): Nothing = {
    throw new AssertionError(
      "Fatal error in the compiler. " + 
      "Unexpected type for an expression (" + msg + ")\n" +
      "Type: " + typ + "\n"
    )
  }
}
