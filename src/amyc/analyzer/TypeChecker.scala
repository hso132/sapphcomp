package amyc
package analyzer

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier

// The type checker for Amy
// Takes a symbolic program and rejects it if it does not follow the Amy typing rules.
object TypeChecker extends Pipeline[(Program, SymbolTable), (Program, SymbolTable)] {

  def run(ctx: Context)(v: (Program, SymbolTable)): (Program, SymbolTable) = {
    import ctx.reporter._

    val (program, table) = v

    case class Constraint(found: Type, expected: Type, pos: Position)

    // Represents a type variable.
    // It extends Type, but it is meant only for internal type checker use,
    //  since no Amy value can have such type.
    case class TypeVariable private (id: Int) extends Type
    object TypeVariable {
      private val c = new UniqueCounter[Unit]
      def fresh(): TypeVariable = TypeVariable(c.next(()))
    }

    // Generates typing constraints for an expression `e` with a given expected type.
    // The environment `env` contains all currently available bindings (you will have to
    //  extend these, e.g., to account for local variables).
    // Returns a list of constraints among types. These will later be solved via unification.
    def genConstraints(e: Expr, expected: Type)(implicit env: Map[Identifier, Type]): List[Constraint] = {
      
      // Integer comparison
      def compConstraints(lhs: Expr, rhs: Expr) = {
        Constraint(BooleanType, expected, e.position) ::
          genConstraints(lhs, IntType) ++
          genConstraints(rhs, IntType)
      }
      def binaryIntConstraints(lhs: Expr, rhs: Expr) = {
        Constraint(IntType, expected, e.position) :: genConstraints(lhs, IntType) ++ genConstraints(rhs,IntType)
      }
      // This helper returns a list of a single constraint recording the type
      //  that we found (or generated) for the current expression `e`
      def topLevelConstraint(found: Type): List[Constraint] =
        List(Constraint(found, expected, e.position))
      
      e match {
        case IntLiteral(_) =>
          topLevelConstraint(IntType)
        case StringLiteral(_) =>
          topLevelConstraint(StringType)
        
        case Variable(name) => 
          Constraint(env.get(name).get, expected, e.position) :: Nil
        case Ite(cond, thenn, elze) =>
          val moreConstraints = genConstraints(cond, BooleanType) ++
            genConstraints(thenn, expected) ++
            genConstraints(elze, expected)
          moreConstraints
        case Equals(lhs, rhs) =>
          // HINT: Take care to implement the specified Amy semantics
          val firstArg = TypeVariable.fresh();
          val secondArg = TypeVariable.fresh();
          val moreConstraints = genConstraints(lhs, firstArg) ++ genConstraints(rhs, secondArg)
          Constraint(firstArg, secondArg, lhs.position) :: 
            Constraint(BooleanType, expected, e.position) ::
            moreConstraints;

        case Neg(arg) =>
          Constraint(IntType, expected, e.position) :: genConstraints(arg,IntType)
        case Let(ParamDef(name, tt), value, body) =>
          val bodyType = TypeVariable.fresh();
          Constraint(bodyType, expected, e.position) :: 
            genConstraints(value, tt.tpe) ++ genConstraints(body, bodyType)(env+(name->tt.tpe))

        case Mod(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)
        case Div(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)
        case Concat(lhs,rhs) => 
          Constraint(StringType, expected, e.position) :: genConstraints(lhs,StringType) ++ genConstraints(rhs,StringType)
        case LessThan(lhs, rhs) =>
          compConstraints(lhs,rhs)
        case LessEquals(lhs, rhs) =>
          compConstraints(lhs,rhs)
        
        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            ???  // TODO
          }

          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            val (patConstraints, moreEnv) = handlePattern(cse.pat, scrutExpected)
            ???  // TODO
          }

          val st = TypeVariable.fresh()
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))
        

        case Error(msg) =>
          val msgConstraints = genConstraints(msg, StringType);
          Constraint(expected,expected, msg.position) :: msgConstraints;
        case Call(qname, args) =>
          val (sig,extraConstraint) = (table.getFunction(qname),table.getConstructor(qname)) match {
            // Function call
            case (Some(sig), None) => (sig, Constraint(sig.retType,expected,e.position))
            case (None,Some(sig)) => (sig, Constraint(table.getType(sig.parent).get, expected, e.position))
            case _ => throw new scala.MatchError(e)
          }
          val types = sig.argTypes
          val moreConstraints = (args zip types).map(pair => genConstraints(pair._1,pair._2));
           moreConstraints.flatten
        case h =>
          throw new scala.MatchError(h)
      }
    }


    // Given a list of constraints `constraints`, replace every occurence of type variable
    //  with id `from` by type `to`.
    def subst_*(constraints: List[Constraint], from: Int, to: Type): List[Constraint] = {
      // Do a single substitution.
      def subst(tpe: Type, from: Int, to: Type): Type = {
        tpe match {
          case TypeVariable(`from`) => to
          case other => other
        }
      }

      constraints map { case Constraint(found, expected, pos) =>
        Constraint(subst(found, from, to), subst(expected, from, to), pos)
      }
    }

    // Solve the given set of typing constraints and
    //  call `typeError` if they are not satisfiable.
    // We consider a set of constraints to be satisfiable exactly if they unify.
    def solveConstraints(constraints: List[Constraint]): Unit = {
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found,expected) match {
            // T = x, T variable
            case (TypeVariable(id1), TypeVariable(id2)) =>

            // T = x, T constant
            case (TypeVariable(id), other) => 
            // Orient
            case (_, TypeVariable(_)) =>
              solveConstraints(Constraint(expected, found, pos) :: more)
            case (type1, type2) =>
              if(type1 == type2) {
                solveConstraints(more)
              }
              else {
                error(s"Type error; expected $expected, found $found", pos)
              }
          }
      }
    }

    // Putting it all together to type-check each module's functions and main expression.
    program.modules.foreach { mod =>
      // Put function parameters to the symbol table, then typecheck them against the return type
      mod.defs.collect { case FunDef(_, params, retType, body) =>
        val env = params.map{ case ParamDef(name, tt) => name -> tt.tpe }.toMap
        solveConstraints(genConstraints(body, retType.tpe)(env))
      }

      // Type-check expression if present. We allow the result to be of an arbitrary type by
      // passing a fresh (and therefore unconstrained) type variable as the expected type.
      val tv = TypeVariable.fresh()
      mod.optExpr.foreach(e => solveConstraints(genConstraints(e, tv)(Map())))
    }

    v

  }
}
