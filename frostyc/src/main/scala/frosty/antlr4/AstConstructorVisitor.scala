package frosty.antlr4

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree._
import frosty.antlr4.FrostyParser._
import frosty.ast._
import frosty.namespace._
import frosty.types._
import frosty.{Position, SourceCode}
import scala.collection.breakOut
import scala.collection.JavaConverters._

/** Disjoint union of the various constructs returned by the visitor. */
private[antlr4] sealed trait VisitResult
private[antlr4] case class VisitProc(proc: Ast) extends VisitResult
private[antlr4] case class VisitType(typ: Type) extends VisitResult
private[antlr4] case class VisitArg(name: String) extends VisitResult
private[antlr4] case class VisitArgs(names: List[String]) extends VisitResult
private[antlr4] case class VisitTypeArgs(args: List[Type]) extends VisitResult
private[antlr4] case class VisitNameType(name: String, typ: Type)
  extends VisitResult
private[antlr4] case class VisitNameTypes(list: List[(String, Type)])
  extends VisitResult
private[antlr4] case class VisitNamespaceAst(nsAst: NamespaceAst[Ast, Type])
  extends VisitResult
private[antlr4] case class VisitAbsolutePath(absPath: AbsolutePath)
  extends VisitResult
private[antlr4] case class VisitRelativePath(relPath: RelativePath)
  extends VisitResult
private[antlr4] case class VisitVisibilityModifier(mod: VisibilityModifier)
  extends VisitResult
private[antlr4] case class VisitSubselector(s: Subselector) extends VisitResult
private[antlr4] case class VisitSelector(s: ImportSelector) extends VisitResult
private[antlr4] case class VisitImportAst(a: ImportAst) extends VisitResult
private[antlr4]
case class VisitEntry(finalResult: List[NamespaceAst[Ast, Type]])
  extends VisitResult

private[frosty] class AstConstructorVisitor(sourceCode: SourceCode)
extends FrostyBaseVisitor[VisitResult] {
  /** Extracts start position of the first token.
    *
    * Depends on `sourceCode`, therefore the method must be nested.
    */
  private def pos(p: ParserRuleContext): Position = {
    val tok = p.start
    val line = tok.getLine
    val col = tok.getCharPositionInLine
    Position(sourceCode, line, col)
  }
  
  private def proc(
    c: ParserRuleContext,
    p: AstC[Ast, (List[String], Ast)]
  ): VisitResult = VisitProc(positionedAst(pos(c), p))

  private def typ(c: ParserRuleContext, t: Type): VisitResult = VisitType(t)
  private def arg(c: ParserRuleContext, a: String): VisitResult =
    VisitArg(a)
  private def args(c: ParserRuleContext, as: List[String]): VisitResult =
    VisitArgs(as)

  /** Drop quotes, interpret escape sequences. */
  private def interpretStringLiteral(lit: String): String = {
    val n = lit.size
    lit
      .substring(1, n - 1)
      .replaceAll("\\\\\\\\", "\\\\") // this just replaces \\ by \
      .replaceAll("\\\\n", "\n")      // this replaces \n by line break
  }

  override def visitEntry(ctx: EntryContext): VisitResult = {
    VisitEntry(ctx.nsAst.asScala.map { nsAstCtx =>
      val VisitNamespaceAst(a) = visit(nsAstCtx)
      a
    }(breakOut))
  }

  override def visitOr(c: OrContext): VisitResult = {
    val VisitProc(a) = visit(c.proc(0))
    val VisitProc(b) = visit(c.proc(1))
    proc(c, Or(a, b))
  }
  override def visitNewProc(c: NewProcContext): VisitResult = {
    val VisitNameTypes(newNamesTypes) = visit(c.nametypelist)
    val (names, types) = newNamesTypes.unzip
    val VisitProc(body) = visit(c.proc)
    proc(c, New(types, (names, body)))
  }
  override def visitAwaitProc(c: AwaitProcContext): VisitResult = {
    val VisitProc(chan) = visit(c.proc)
    proc(c, Await(chan))
  }
  override def visitInvocation(c: InvocationContext): VisitResult = {
    val VisitProc(p) = visit(c.proc)
    val sendables: List[Ast] = (for (s <- c.sendable.asScala) yield {
      val VisitProc(x) = visit(s)
      x
    })(breakOut)
    proc(c, Invocation(p, sendables))
  }
  override def visitMulDivRem(c: MulDivRemContext): VisitResult = {
    val VisitProc(a) = visit(c.proc(0))
    val VisitProc(b) = visit(c.proc(1))
    if (c.op.getType == MUL) proc(c, Mul(a, b))
    else if (c.op.getType == DIV) proc(c, Div(a, b))
    else proc(c, Rem(a, b))
  }
  override def visitAddSub(c: AddSubContext): VisitResult = {
    val VisitProc(a) = visit(c.proc(0))
    val VisitProc(b) = visit(c.proc(1))
    if (c.op.getType == ADD) proc(c, Add(a, b))
    else proc(c, Sub(a, b))
  }
  override def visitComparison(c: ComparisonContext): VisitResult = {
    val VisitProc(a) = visit(c.proc(0))
    val VisitProc(b) = visit(c.proc(1))
    if (c.op.getType == LE) proc(c, Le(a, b))
    else if (c.op.getType == LEQ) proc(c, Leq(a, b))
    else if (c.op.getType == GEQ) proc(c, Geq(a, b))
    else if (c.op.getType == GR) proc(c, Gr(a, b))
    else throw new Error(
      "Internal compiler error: unexpected comparison operator " + 
      c.op.getType +
      " (probably caused by recent grammar changes)."
    )
  }
  override def visitParens(c: ParensContext): VisitResult = visit(c.proc)
  override def visitFreeze(c: FreezeContext): VisitResult = {
    val VisitProc(p) = visit(c.proc)
    proc(c, Freeze(p))
  }
  override def visitUnfreeze(c: UnfreezeContext): VisitResult = {
    val VisitProc(p) = visit(c.proc)
    proc(c, Unfreeze(p))
  }
  override def visitIfElse(c: IfElseContext): VisitResult = {
    val VisitProc(i) = visit(c.proc(0))
    val VisitProc(t) = visit(c.proc(1))
    val VisitProc(e) = visit(c.proc(2))
    proc(c, IfElse(i, t, e))
  }
  override def visitVar(c: VarContext): VisitResult =
    VisitProc(positionedVariable(pos(c), c.ID.getText))
  override def visitTrue(c: TrueContext): VisitResult = proc(c, B(true))
  override def visitFalse(c: FalseContext): VisitResult = proc(c, B(false))
  override def visitString(c: StringContext): VisitResult = 
    proc(c, S(interpretStringLiteral(c.STR_LIT.getText)))
  override def visitUnit(c: UnitContext): VisitResult = proc(c, UnitValue)
  override def visitEq(c: EqContext): VisitResult = {
    val VisitProc(a) = visit(c.proc(0))
    val VisitProc(b) = visit(c.proc(1))
    proc(c, Eq(a, b))
  }
  override def visitInt(c: IntContext): VisitResult = 
    proc(c, I(c.INT.getText))
  override def visitNeg(c: NegContext): VisitResult = {
    val VisitProc(a) = visit(c.proc)
    proc(c, Neg(a))
  }
  override def visitParProc(c: ParProcContext): VisitResult = visit(c.par)
  override def visitNot(c: NotContext): VisitResult = {
    val VisitProc(a) = visit(c.proc)
    proc(c, Not(a))
  }
  override def visitAnd(c: AndContext): VisitResult = {
    val VisitProc(a) = visit(c.proc(0))
    val VisitProc(b) = visit(c.proc(1))
    proc(c, And(a, b))
  }
  override def visitAscription(c: AscriptionContext): VisitResult = {
    val VisitProc(p) = visit(c.proc)
    val VisitType(t) = visit(c.typ)
    proc(c, Ascription(p, t))
  }
  override def visitContractProc(c: ContractProcContext): VisitResult = {
    visit(c.contract)
  }
  override def visitDefProc(c: DefProcContext): VisitResult = {
    visit(c.funcdef)
  }
  override def visitPars(c: ParsContext): VisitResult = {
    val procs: List[Ast] = (for (a <- c.proc.asScala) yield {
      val VisitProc(p) = visit(a)
      p
    })(breakOut)
    proc(c, Parallel(procs))
  }
  override def visitPar(c: ParContext): VisitResult = visit(c.pars)
  override def visitSendableProc(c: SendableProcContext): VisitResult =
    visit(c.proc)
  override def visitSendablePars(c: SendableParsContext): VisitResult =
    visit(c.pars)
  override def visitSendProc(c: SendProcContext): VisitResult = {
    val VisitProc(chan) = visit(c.proc)
    val sendables: List[Ast] = (for (sCtx <- c.sendable.asScala) yield {
      val VisitProc(s) = visit(sCtx)
      s
    })(breakOut)
    proc(c, Tell(chan, sendables))
  }
  override def visitArg(c: ArgContext): VisitResult = {
    val name = c.ID.getText
    arg(c, name)
  }
  override def visitArglist(c: ArglistContext): VisitResult = {
    val args: List[String] = (for (aCtx <- c.arg.asScala) yield {
      val VisitArg(x) = visit(aCtx)
      x
    })(breakOut)
    VisitArgs(args)
  }
  override def visitReceiveProc(c: ReceiveProcContext): VisitResult = {
    val VisitProc(chan) = visit(c.proc(0))
    val VisitProc(body) = visit(c.proc(1))
    val VisitArgs(names) = visit(c.arglist)
    proc(c, Receive(chan, (names, body)))
  }
  override def visitTyparglist(c: TyparglistContext): VisitResult = {
    val types: List[Type] = (for (tCtx <- c.typ.asScala) yield {
      val VisitType(t) = visit(tCtx)
      t
    })(breakOut)
    VisitTypeArgs(types)
  }
  override def visitTypeInt(c: TypeIntContext): VisitResult = 
    typ(c, IntType)
  override def visitTypeRun(c: TypeRunContext): VisitResult = {
    val VisitTypeArgs(types) = visit(c.typarglist)
    typ(c, RunnableType(types))
  }
  override def visitTypeUnit(c: TypeUnitContext): VisitResult =
    typ(c, UnitType)
  override def visitTypeFrozenProc(c: TypeFrozenProcContext): VisitResult = 
    typ(c, FreezeProcessType)
  override def visitTypeFunction(c: TypeFunctionContext): VisitResult = {
    val VisitTypeArgs(types) = visit(c.typarglist)
    // TODO: what happens if there are less than two types?
    typ(c, FunctionType(types.init, types.last))
  }
  override def visitTypeBool(c: TypeBoolContext): VisitResult =
    typ(c, BooleanType)
  override def visitTypeString(c: TypeStringContext): VisitResult =
    typ(c, StringType)
  override def visitTypeChan(c: TypeChanContext): VisitResult = {
    val VisitTypeArgs(types) = visit(c.typarglist)
    typ(c, ChannelType(types))
  }
  override def visitContract(c: ContractContext): VisitResult = {
    val VisitProc(name) = visit(c.proc(0))
    val VisitArgs(args) = visit(c.arglist)
    val VisitProc(body) = visit(c.proc(1))
    proc(c, Contract(name, (args, body)))
  }
  override def visitFuncdef(c: FuncdefContext): VisitResult = {
    val VisitProc(name) = visit(c.proc(0))
    val VisitArgs(args) = visit(c.arglist)
    val VisitProc(body) = visit(c.proc(1))
    proc(c, Def(name, (args, body)))
  }

  override def visitAbsolutePath(ctx: AbsolutePathContext): VisitResult = {
    VisitAbsolutePath(AbsolutePath(ctx.ID().asScala.toList.map(_.getText)))
  }

  override def visitCode(ctx: CodeContext): VisitResult = {
    val VisitProc(p) = visit(ctx.proc)
    VisitNamespaceAst(Code(p, pos(ctx)))
  }
  override def visitDecl(ctx: DeclContext): VisitResult = {
    val VisitVisibilityModifier(mod) = visit(ctx.visibilityModifier)
    val name = ctx.ID.getText
    val VisitType(typ) = visit(ctx.typ)
    VisitNamespaceAst(Declaration(mod, name, typ, pos(ctx)))
  }
  override def visitImportAbs(ctx: ImportAbsContext): VisitResult = {
    val pathComponents: List[String] = ctx.ID.asScala.map(_.getText)(breakOut)
    val VisitSelector(sel) = visit(ctx.importSel)
    VisitImportAst(ImportAbsAst(AbsolutePath(pathComponents), sel, pos(ctx)))
  }
  override def visitImportRel(ctx: ImportRelContext): VisitResult = {
    val pathComponents: List[String] = ctx.ID.asScala.map(_.getText)(breakOut)
    val VisitSelector(sel) = visit(ctx.importSel)
    VisitImportAst(ImportRelAst(RelativePath(pathComponents), sel, pos(ctx)))
  }
  override def visitNametype(ctx: NametypeContext): VisitResult = {
    val VisitType(t) = visit(ctx.typ)
    VisitNameType(ctx.ID.getText, t)
  }

  override def visitNametypelist(ctx: NametypelistContext): VisitResult = {
    val nts: List[(String, Type)] = ctx.nametype.asScala.map{ ntCtx =>
      val VisitNameType(n, t) = visit(ntCtx)
      (n, t)
    }(breakOut)
    VisitNameTypes(nts)
  }

  override def visitNsAst(ctx: NsAstContext): VisitResult = {
    visit(ctx.getChild(0))
  }
  override def visitPkg(ctx: PkgContext): VisitResult = {
    val VisitVisibilityModifier(m) = visit(ctx.visibilityModifier)
    val content: List[NamespaceAst[Ast, Type]] = ctx.nsAst.asScala.map { c =>
      val VisitNamespaceAst(a) = visit(c)
      a
    }(breakOut)
    VisitNamespaceAst(Pkg(m, ctx.ID.getText, content, pos(ctx)))
  }
  override def visitAbsoluteName(ctx: AbsoluteNameContext): VisitResult = {
    val VisitAbsolutePath(r) = visit(ctx.absolutePath)
    proc(ctx, AbsoluteName(r))
  }
  override def visitRelativeName(ctx: RelativeNameContext): VisitResult = {
    val VisitRelativePath(r) = visit(ctx.relativePath)
    proc(ctx, RelativeName(r))
  }
  override def visitRelativePath(ctx: RelativePathContext): VisitResult = {
    VisitRelativePath(RelativePath(ctx.ID().asScala.toList.map(_.getText)))
  }
  override def visitSelectAll(ctx: SelectAllContext): VisitResult = {
    VisitSelector(SelectAll)
  }
  override def visitSelectOne(ctx: SelectOneContext): VisitResult = {
    VisitSelector(SelectOne(ctx.ID.getText))
  }
  override def visitSelectSubsels(ctx: SelectSubselsContext): VisitResult = {
    val sSels: List[Subselector] =
      ctx
      .importSubsel
      .asScala
      .map{ x => val VisitSubselector(sSel) = visit(x); sSel }(
        breakOut
      )
    VisitSelector(SelectSubselectors(sSels))
  }
  override def visitSubselectOne(ctx: SubselectOneContext): VisitResult = {
    VisitSubselector(SubselectOne(ctx.ID.getText))
  }
  override def visitIgnoreOne(ctx: IgnoreOneContext): VisitResult = {
    VisitSubselector(IgnoreOne(ctx.ID.getText))
  }
  override def visitSubselectRest(ctx: SubselectRestContext): VisitResult = {
    VisitSubselector(SubselectRest)
  }
  override def visitSubselectRenameOne(ctx: SubselectRenameOneContext)
  : VisitResult = {
    VisitSubselector(SubselectRenameOne(ctx.ID(0).getText, ctx.ID(1).getText))
  }
  override def visitUnderImport(ctx: UnderImportContext): VisitResult = {
    val VisitImportAst(i) = visit(ctx.importClause)
    val content = ctx.nsAst.asScala.toList.map(visit).collect {
      case VisitNamespaceAst(n) => n
    }
    VisitNamespaceAst(UnderImport(i, content))
  }
  override def visitVisibilityModifier(ctx: VisibilityModifierContext)
  : VisitResult = {
    VisitVisibilityModifier(
      ctx.keyword.getType match {
        case FrostyParser.PUBLIC => Public
        case FrostyParser.PRIVATE => Private
        case sthElse => throw new AssertionError(
          "Unexpected visibility modifier '" + sthElse + "'"
        )
      }
    )
  }
} 