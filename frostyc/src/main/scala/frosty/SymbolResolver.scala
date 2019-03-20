package frosty

import cats.Bitraverse
import cats.Id
import cats.Monoid
import cats.data.Chain
import cats.data.Validated
import cats.data.Validated.{Valid, Invalid}
import cats.instances.list._
import cats.syntax.validated._
import cats.syntax.traverse._
import cats.syntax.applicative._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.monoid._
import frosty.algebra.{~~>, Term}
import frosty.ast._
import frosty.namespace._
import frosty.types._
import frosty.util.PipeForward
import frosty.algebra.missingCat.productMonoid
import scala.language.higherKinds

object SymbolResolver extends Phase[
  List[NamespaceAst[Ast, Type]],
  (List[SymbolDeclaration], List[Ast])
](
  PhaseName.Resolve,
  "Replaces all relative paths (names) by absolute paths."
) {

  def apply(input: List[NamespaceAst[Ast, Type]])
  : CompilationErrorsOr[(List[SymbolDeclaration], List[Ast])] = {
    
    constructGlobalView(input)
    .pipeForward {
      case Invalid(errs) => Invalid(errs)
      case Valid(rootPkg) => {
        input
        .map(resolveNamesInOneCompilationUnit(_, rootPkg))
        .sequence
        .map { listOfListAst => 
          val (symbLists, astLists) = listOfListAst.unzip
          val allSymbDecls = symbLists.flatten
          val astList = astLists.flatten
          val firstPos = position(astList.head)
          (allSymbDecls, astList)
        }
      }
    }
    .toPhaseErrors
  }

  def resolveNamesInOneCompilationUnit(
    nast: NamespaceAst[Ast, Type],
    globalPackage: GlobPackage
  ): ValidationErrorsOr[(List[SymbolDeclaration], List[Ast])] = {

    /* A little bit of infrastructure and syntactic sugar to make the 
     * accumulation of declarations and ASTs a little less painful.
     */
    type Accumulator = (Chain[SymbolDeclaration], Chain[Ast])

    implicit class AccumulatorLeftOps(s: SymbolDeclaration) {
      def acc: Accumulator = (Chain(s), Chain.empty)
    }

    implicit class AccumulatorRightOps(a: Ast) {
      def acc: Accumulator = (Chain.empty, Chain(a))
    }

    /* Recursively traverse the namespace-ast, keeping track of the 
     * current view (modifying it accordingly when entering packages or 
     * importing symbols). Collect all declarations and ASTs (with all
     * relative names replaced by global ones).
     */
    def rec(view: View, nast: NamespaceAst[Ast, Type])
    : ValidationErrorsOr[Accumulator] = nast match {

      case Declaration(mod, name, typ, pos) => {
        view
        .local
        .lookup(name) // : Option[Global]
        .map {
          case GlobSymbol(absPath, pos) =>
            // TODO: why am I actually accumulating it? I've already accumulated
            // it while constructing the global view. Maybe refactor the whole
            // method, and few methods around it (later).
            SymbolDeclaration(absPath, typ, pos).acc.pure[ValidationErrorsOr]

          case GlobPackage(_, _) =>
            err(pos, s"Expected symbol for name `$name`, but found a package")
        }
        .getOrElse(err(pos, "Could not resolve declaration name: " + name))
      }

      case Code(ast, pos) => {
        resolveNamesInAst(
          ast,
          relPath => view.local.resolve(relPath).collect{
            case GlobSymbol(p, _) => p
          }
        )
        .map(_.acc)
      }

      case Pkg(_mod, name, content, pos) => {
        view
        .enter(name)
        .pipeForward {
          case None => err(pos, "No such package: " + name)
          case Some(pkgView) => {
            content
            .map(rec(pkgView, _))
            .sequence
            .map(_.foldLeft(Monoid[Accumulator].empty)(_ |+| _))
          }
        }
      }

      case UnderImport(i, content) => i match {
        case ImportRelAst(path, sel, pos) => {
          view
          .importRel(path, sel)
          .map(rec(_, content))
          .getOrElse(err(pos, "Failed import: " + i))
        }

        case ImportAbsAst(path, sel, pos) => {
          view
          .importAbs(path, sel)
          .map(rec(_, content))
          .getOrElse(err(pos, "Failed import: " + i))
        }
      }
    }

    // View passed to the top-level of the recursion
    val rootView = View.fromGlobal(globalPackage)
    
    rec(rootView, nast)
    .map { case (cd, ca) => 
      // convert chains to lists
      (cd.toList, ca.toList)
    }
  }


  /** Converts list of `Global` namespace elements into
    * a map, and then wraps it into a `GlobPackage`.
    * 
    * If there are multiple subpackage-blocks with same
    * name, then their contents are merged. If there are
    * multiple symbols with same name, an error is raised.
    */
  private def combinePackageContent(
    content: List[(VisibilityModifier, String, Global)], 
    pos: Position
  ): ValidationErrorsOr[GlobPackage] = {

    def combineHelper(sameName: List[(VisibilityModifier, String, Global)])
    : ValidationErrorsOr[(VisibilityModifier, String, Global)] = {
      if (sameName.size == 1) {
        sameName.head.pure[ValidationErrorsOr]
      } else if (
        // all elements with same name are actually the same package, 
        // re-entered multiple times
        sameName.forall { _._3 match {
          case GlobPackage(_, _) => true ; case _ => false 
        }}
      ) {
          val allModifiers = sameName.map(_._1).toSet

          if (allModifiers.size > 1) {
            err(
              sameName.head._3.position,
              "Inconsistent visibility modifiers: both `public` " +
              "and `private` modifiers found for package " + sameName.head._2
            )
          } else {
            val content = for {
              (_, _, GlobPackage(c, _)) <- sameName
              (n, (m, v)) <- c
            } yield (m, n, v)

            val firstPos = sameName.head._3.position
            combinePackageContent(content, firstPos).map { pkg => 
              (
                allModifiers.head, // common visibility
                sameName.head._2,  // common name
                pkg                // combined package
              )
            }
          }
      } else {
        // if the elements are not the same package, then they are not 
        // combinable at all.
        err(
          sameName.head._3.position,
          "Name collision for name `" + sameName.head._2 + "`"
        )
      }
    }

    content
    .groupBy(_._2)
    .mapValues(combineHelper)
    .mapValues{ _.map{ case (m, n, g) => (m, g) } }
    .toList
    .map { x => x._2.map { kv => (x._1, kv) } }
    .sequence[ValidationErrorsOr, (String, (VisibilityModifier, Global))]
    .map(cs => GlobPackage(cs.toMap, pos))
  }

  /** Traverses all asts from all compilation units, and 
    * combines all namespaces, packages, and symbol names
    * into a single global view.
    */
  private def constructGlobalView(input: List[NamespaceAst[Ast, Type]])
  : ValidationErrorsOr[GlobPackage] = {
    
    /** Transforms a `NamespaceAst`-substructure into 
      * a sequence of elements that look like the content of a `Global`.
      * 
      * Note that both errors and useful results are accumulated in a `Chain`,
      * the `Chain` in the return type is not the same as the `Chain` for
      * accumulating errors.
      */
    def helperRec(
      currPath: AbsolutePath,
      substructure: NamespaceAst[Ast, Type]
    ): ValidationErrorsOr[Chain[(VisibilityModifier, String, Global)]] = {
      substructure match {

        case Declaration(mod, name, _decl, pos) =>
          Chain((mod, name, GlobSymbol(currPath / name, pos)))
          .pure[ValidationErrorsOr]

        case Code(_, _) => Chain.empty.pure[ValidationErrorsOr]

        case Pkg(mod, name, content, pos) => {
          val subPath = currPath / name
          content.traverse(helperRec(subPath, _)) match {

            case Valid(list) => 
              combinePackageContent(list.flatMap(_.toList), pos)
              .map(pkg => Chain((mod, name, pkg)))

            case Invalid(errs) => Invalid(errs)
          }
        }
        case UnderImport(_, c) => helperRec(currPath, c)
      }
    }

    input
    .map(helperRec(AbsolutePath(Nil), _))
    .sequence
    .map(_.flatMap(_.toList))
    .pipeForward {
      // that would usually be a single `.flatMap`, but `Validated` 
      // provides none, apparently to prevent accidental short-circuiting
      // instead of error accumulation.
      case Valid(list) => combinePackageContent(list, input.head.position)
      case Invalid(errs) => Invalid(errs)
    }

  }

  /** Tries to rewrite a single `Ast` by replacing all
    * relative paths with absolute paths.
    *
    * The namespace environment (packages, imports etc.) 
    * does not change during the traversal.
    */
  private def resolveNamesInAst(
    ast: Ast,
    resolve: RelativePath => Option[AbsolutePath]
  ): ValidationErrorsOr[Ast] = {
    
    /* Attempts to resolve a relative path into an 
     * AbsolutePath, raises compilation error on failure.
     */
    def resolveVal(rp: RelativePath, pos: Position)
    : ValidationErrorsOr[AbsolutePath] = resolve(rp) match {
      case None => err(pos, "Name could not be resolved: " + rp)
      case Some(ap) => ap.valid
    }

    // Positioned Ast with `Validation`-stuff inside and outside
    type PAVV[X, Y] = 
      PositionedAstC[ValidationErrorsOr[X], ValidationErrorsOr[Y]]
    type VPA[X, Y] = ValidationErrorsOr[PositionedAstC[X, Y]]

    /* Almost the same as the ordinary `sequence` on `PAVV`, but
     * handles `RelativeName`s specially.
     * 
     * Essentially, swaps `PositionedAstC` and `Validation`.
     */
    val resolveRelativeNames = new (PAVV ~~> VPA) {
      def apply[X, Y](pAst: PAVV[X, Y]): VPA[X, Y] = {
        val (pos, ast) = pAst
        ast match {
          case RelativeName(relPath) =>
            resolveVal(relPath, pos)
            .map(p => (pos, AbsoluteName(p)))
          case sthElse => 
            Bitraverse[PositionedAstC]
            .bisequence[ValidationErrorsOr, X, Y](pAst)
        }
      }
    }

    Term.modifyShapeA(ast)(resolveRelativeNames)
  }

  /** Blames existence of `RelativePath`s in later phases on this object.
    * 
    * This is supposed to help exclude certain cases in lengthy pattern matches,
    * without sacrificing exhaustiveness checks.
    */
  def blameFor(relName: RelativeName): Nothing = {
    throw new AssertionError(
      "Fatal compiler error: SymbolResolver failed to eliminate " + 
      "relative path " + relName + " during the name resolution phase."
    )
  }
}
