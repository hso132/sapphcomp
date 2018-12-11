package amyc
package codegen

import analyzer._
import ast.Identifier
import ast.SymbolicTreeModule.{Call => AmyCall, Div => AmyDiv, And => AmyAnd, Or => AmyOr, _}
import utils.{Context, Pipeline}
import wasm._
import Instructions._
import Utils._

// Generates WebAssembly code for an Amy program
object CodeGen extends Pipeline[(Program, SymbolTable), Module] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Module = {
    val (program, table) = v

    // Generate code for an Amy module
    def cgModule(moduleDef: ModuleDef): List[Function] = {
      val ModuleDef(name, defs, optExpr) = moduleDef
      // Generate code for all functions
      defs.collect { case fd: FunDef if !builtInFunctions(fullName(name, fd.name)) =>
        cgFunction(fd, name, false)
      } ++
      // Generate code for the "main" function, which contains the module expression
      optExpr.toList.map { expr =>
        val mainFd = FunDef(Identifier.fresh("main"), Nil, TypeTree(IntType), expr)
        cgFunction(mainFd, name, true)
      }
    }

    // Generate code for a function in module 'owner'
    def cgFunction(fd: FunDef, owner: Identifier, isMain: Boolean): Function = {
      // Note: We create the wasm function name from a combination of
      // module and function name, since we put everything in the same wasm module.
      val name = fullName(owner, fd.name)
      Function(name, fd.params.size, isMain){ lh =>
        val locals = fd.paramNames.zipWithIndex.toMap
        val body = cgExpr(fd.body)(locals, lh)
        if (isMain) {
          body <:> Drop // Main functions do not return a value,
                        // so we need to drop the value generated by their body
        } else {
          body
        }
      }
    }

    // Generate code for an expression expr.
    // Additional arguments are a mapping from identifiers (parameters and variables) to
    // their index in the wasm local variables, and a LocalsHandler which will generate
    // fresh local slots as required.
    def cgExpr(expr: Expr)(implicit locals: Map[Identifier, Int], lh: LocalsHandler): Code = {

      def genCodeForCall(name: Identifier, args: List[Expr]): Code = {
        getSig(name) match {
          case FunSig(_, _, owner) =>
            val argsInstructions: Code = for(arg <- args) yield cgExpr(arg)
            argsInstructions <:> Call(fullName(owner, name))
          case ConstrSig(_, _, index) =>
            genCodeForConstructor(index, args)
        }
      }

      // Extracts the signature from the name
      def getSig(name: Identifier): Signature[_] = {
        ((table.getFunction(name), table.getConstructor(name)): @unchecked) match {
          case (Some(sig), None) => sig;
          case (None, Some(sig)) => sig;
        }
      }

      // Generates code that creates an adt value,
      // writes it to the memory, and leaves the pointer on
      // the stack
      def genCodeForConstructor(idx: Int, args: List[Expr]): Code = {
        val oldBoundary = lh.getFreshLocal();
        val argsCode: Code = for((arg,argIndex) <- args.zipWithIndex) yield {
          // Store the current argument in the current memory boundary
          GetLocal(oldBoundary) <:>
          Const(4*argIndex+4) <:>
          Add <:>
          cgExpr(arg) <:>
          Store
        }

        // Save the old boundary, and keep a variable for the new one
        GetGlobal(memoryBoundary) <:>
        SetLocal(oldBoundary) <:>
        // Increase boundary
        GetGlobal(memoryBoundary) <:>
        Const(4*args.length+4) <:>
        Add <:>
        SetGlobal(memoryBoundary) <:>
        // Repeat for all arguments
        GetLocal(oldBoundary) <:>
        Const(idx) <:>
        Store <:>
        argsCode <:>
        // Get the pointer to the beginning of the adt
        GetLocal(oldBoundary)
      }

      def cgBinOp(lhs: Expr, rhs: Expr, binOp: Instruction): Code = {
        cgExpr(lhs) <:> 
        cgExpr(rhs) <:>
        binOp
      }

      def cgLit(lit: Literal[_]): Code = {
        lit match {
          case IntLiteral(i) =>
            Const(i)
          case BooleanLiteral(b) => Const(if(b) 1 else 0)
          case UnitLiteral() => Const(0)
          case StringLiteral(s) => mkString(s)
        }
      }

      def cgBranch(cond: Code, thenn: Code, elze: Code): Code = {
        // Branches generated by users always end with some expression
        // even if that expression is just unit
        cond <:>
        If_i32 <:>
        thenn <:>
        Else <:>
        elze <:>
        End
      }

      def cgMatch(scrut: Expr, cases: List[MatchCase]): Code = {
        // Expects one argument; the one to match on
        // Leaves a boolean on the stack
        // Only sets locals if it returns true
        def matchAndBind(pat: Pattern): (Code, Map[Identifier, Int]) = {
          pat match {
            case WildcardPattern() => (Const(1), Map())
            case IdPattern(name) => (Const(1), Map((name -> lh.getFreshLocal())))
            case LiteralPattern(lit) => (cgExpr(lit) <:> Eq, Map())
            case CaseClassPattern(constr, args) =>
              val ctr = table.getConstructor(constr).get;
              val ctrIdx = ctr.index;
              val numArgs = ctr.argTypes.length
              val ctrPtrLocal = lh.getFreshLocal()
              val potentialRest: List[(Code, Map[Identifier, Int])] =
                for((arg,idx) <- args.zipWithIndex) yield {
                  val (code, moreLocals) = matchAndBind(arg)
                  (
                    // Get the field to match
                    GetLocal(ctrPtrLocal) <:>
                    adtField(idx) <:>
                    // match it
                    code
                  , moreLocals)
                }

              if(potentialRest.isEmpty) {
                (Const(1), Map())
              } else {
                val (splitCode, splitLocals) = potentialRest.unzip
                val code: Code = splitCode.reduce((c1, c2) => c1 <:> c2 <:> And)
                val moreLocals = splitLocals.reduce((m1,m2) => m1 ++ m2)
                (
                  // Save the pointer
                  SetLocal(ctrPtrLocal) <:>
                  GetLocal(ctrPtrLocal) <:>
                  // Get the index
                  Load <:>
                  // Compare it to the expected index
                  Const(ctrIdx) <:>
                  Eq <:>
                  // Check if the rest of the pattern match
                  If_i32 <:>
                  code <:>
                  Else <:>
                  // Wrong constructor
                  Const(0) <:>
                  End, moreLocals)
              }
          }
        }
        val scrutLocal = lh.getFreshLocal()
        val codeMatchTests: Code = for(cse <- cases) yield {
          val res = matchAndBind(cse.pat);
          val checkCode: Code = res._1;
          val bindings: Map[Identifier, Int] = res._2;
          // This code leaves true on the stack if the pattern matched
          GetLocal(scrutLocal) <:>
          checkCode <:>
          If_i32 <:>
          cgExpr(cse.expr)(locals ++ bindings, lh) <:>
          Else 
        }
        cgExpr(scrut) <:>
        SetLocal(scrutLocal) <:>
        codeMatchTests <:>
        // codeMatchTests ends with an else block
        mkString("Match Error") <:>
        // error
        End

      }

      expr match {
        case AmyCall(name, args) =>
          genCodeForCall(name, args) 

        // Control Structures
        case Sequence(e1, e2) =>
          cgExpr(e1) <:>
          Drop <:> // A sequence is used for its side effects
          cgExpr(e2)

        case Match(scrut, cases) =>
          cgMatch(scrut, cases)
        case Let(df, value, body) =>
          val newVal = lh.getFreshLocal()
          cgExpr(value) <:>
          SetLocal(newVal) <:>
          cgExpr(body)(locals + (df.name -> newVal), lh)
        case Ite(cond, thenn, elze) =>
          val condCode = cgExpr(cond)
          val thenCode = cgExpr(thenn)
          val elseCode = cgExpr(elze)
          cgBranch(condCode, thenCode, elseCode)

          // Leaves
        case Variable(v) =>
          GetLocal(locals(v))
        case lit: Literal[_] =>
          cgLit(lit)
          // Unary operators (all two of them)
        case Neg(op) =>
          // No clue if this is a hack or how it should be done
          Const(0) <:>
          cgExpr(op) <:>
          Sub
        case Not(op) =>
          cgExpr(op) <:>
          Eqz
          // Binary operators 
        case Plus(lhs,rhs) =>
          cgBinOp(lhs, rhs, Add)
        case Minus(lhs, rhs) =>
          cgBinOp(lhs, rhs, Sub)
        case Times(lhs, rhs) =>
          cgBinOp(lhs, rhs, Mul)
        case AmyDiv(lhs, rhs) =>
          cgBinOp(lhs, rhs, Div)
        case Mod(lhs, rhs) =>
          cgBinOp(lhs, rhs, Rem)
        case LessThan(lhs, rhs) =>
          cgBinOp(lhs, rhs, Lt_s)
        case LessEquals(lhs, rhs) =>
          cgBinOp(lhs, rhs, Le_s)
        case Equals(lhs, rhs) =>
          cgBinOp(lhs, rhs, Eq)
        case AmyAnd(lhs, rhs) =>
          val lhsCode = cgExpr(lhs)
          val rhsCode = cgExpr(rhs)
          cgBranch(lhsCode, rhsCode, Const(0))

        case AmyOr(lhs, rhs) =>
          val lhsCode = cgExpr(lhs)
          val rhsCode = cgExpr(rhs)
          cgBranch(lhsCode, Const(1), rhsCode)


        case Concat(lhs, rhs) =>
          cgExpr(lhs) <:> 
          cgExpr(rhs) <:>
          Call("String_concat")
      }
    }

    Module(
      program.modules.last.name.name,
      defaultImports,
      globalsNo,
      wasmFunctions ++ (program.modules flatMap cgModule)
    )

  }
}
