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
      assert(e.hasPosition)
      def lit2Type[T](lit: Literal[T]): Type = {
        lit match {
          case IntLiteral(_) => IntType
          case BooleanLiteral(_) => BooleanType
          case StringLiteral(_) => StringType
          case UnitLiteral() => UnitType
        }
      }

      def binaryConstraints(lhs: Expr, rhs: Expr, resType: Type, argType: Type): List[Constraint] = {
        val fstConstraint = Constraint(resType, expected, e.position)
        val lhsConstraints = genConstraints(lhs, argType)
        val rhsConstraints = genConstraints(rhs, argType)
        fstConstraint :: lhsConstraints ++ rhsConstraints
      }
      // Integer comparison
      def compConstraints(lhs: Expr, rhs: Expr) = {
        binaryConstraints(lhs, rhs, BooleanType, IntType)
      }

      def binaryIntConstraints(lhs: Expr, rhs: Expr) = {
        binaryConstraints(lhs, rhs, IntType, IntType)
      }

      def binaryBooleanConstraints(lhs: Expr, rhs: Expr) = {
        binaryConstraints(lhs, rhs, BooleanType, BooleanType)
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

        // Unary operations
        case Neg(arg) =>
          Constraint(IntType, expected, e.position) :: genConstraints(arg,IntType)
        case Not(arg) =>
          Constraint(BooleanType, expected, e.position) :: genConstraints(arg, BooleanType)
        case Let(ParamDef(name, tt), value, body) =>
          val bodyType = TypeVariable.fresh();
          Constraint(bodyType, expected, e.position) :: 
            genConstraints(value, tt.tpe) ++ genConstraints(body, bodyType)(env+(name->tt.tpe))


        // Binary operations on integers
        case Times(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)
        case Mod(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)
        case Div(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)
        case Plus(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)
        case Minus(lhs, rhs) =>
          binaryIntConstraints(lhs,rhs)

        // Concatenation
        case Concat(lhs,rhs) => 
          Constraint(StringType, expected, e.position) :: genConstraints(lhs,StringType) ++ genConstraints(rhs,StringType)

        // Comparision constraints
        case LessThan(lhs, rhs) =>
          compConstraints(lhs,rhs)
        case LessEquals(lhs, rhs) =>
          compConstraints(lhs,rhs)

        // Equality. RHS and LHS have to evaluate to the same type
        case Equals(lhs, rhs) =>
          val commonType = TypeVariable.fresh
          binaryConstraints(lhs, rhs, BooleanType, commonType)

        case And(lhs, rhs) =>
          binaryBooleanConstraints(lhs, rhs)
        case Or(lhs, rhs) =>
          binaryBooleanConstraints(lhs, rhs)

        // Sequence; only the second type needs to match expected
        case Sequence(e1,e2) =>
          val e1Type = TypeVariable.fresh;
          val e2Type = TypeVariable.fresh;
          Constraint(e2Type, expected, e2.position) :: genConstraints(e1, e1Type) ++ genConstraints(e2, e2Type)

        case Match(scrut, cases) =>
          // Returns additional constraints from within the pattern with all bindings
          // from identifiers to types for names bound in the pattern.
          // (This is analogous to `transformPattern` in NameAnalyzer.)
          def handlePattern(pat: Pattern, scrutExpected: Type):
            (List[Constraint], Map[Identifier, Type]) =
          {
            pat match {
              case WildcardPattern() => (List(),Map())  
              case IdPattern(name) =>
                val fresh = TypeVariable.fresh();
                (List(Constraint(fresh, scrutExpected, pat.position)), Map(name -> fresh))
              case CaseClassPattern(constr, args) =>
                // Constructor is guarenteed by previous step to exist
                val ctrSig = table.getConstructor(constr).get
                val expectedTypes = ctrSig.argTypes
                val actualParent: Type = ClassType(ctrSig.parent)
                val innerPatterns = args zip expectedTypes map(x => handlePattern(x._1, x._2))
                val innerConstraints: List[Constraint] = innerPatterns.unzip._1.flatten;
                val innerBindings: Map[Identifier, Type] = innerPatterns.unzip._2.map(_.toList).flatten.toMap;
                val newConstraint = Constraint(actualParent, scrutExpected, pat.position);
                (newConstraint :: innerConstraints,
                  innerBindings)
              case LiteralPattern(lit) => {
                (List(Constraint(lit2Type(lit), scrutExpected, pat.position)), Map())
              }
            }
          }


          def handleCase(cse: MatchCase, scrutExpected: Type): List[Constraint] = {
            assert(cse.hasPosition)
            val (patConstraints, moreEnv): (List[Constraint], Map[Identifier, Type]) = handlePattern(cse.pat, scrutExpected)
            patConstraints ::: genConstraints(cse.expr, expected)(env ++ moreEnv)
          }

          val st = TypeVariable.fresh()
          assert(cases.forall(_.hasPosition))
          genConstraints(scrut, st) ++ cases.flatMap(cse => handleCase(cse, st))
        

        case Error(msg) =>
          val msgConstraints = genConstraints(msg, StringType);
          Constraint(expected,expected, msg.position) :: msgConstraints;
        case lit: Literal[_] =>
          Constraint(lit2Type(lit), expected, lit.position) :: Nil

        case Call(qname, args) =>
          val (sig,extraConstraint) = (table.getFunction(qname),table.getConstructor(qname)) match {
            // Function call
            case (Some(sig), None) => (sig, Constraint(sig.retType,expected,e.position))
            case (None,Some(sig)) => (sig, Constraint(ClassType(sig.parent), expected, e.position))
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
      //println(constraints.length)
      //constraints.foreach(println)
      constraints match {
        case Nil => ()
        case Constraint(found, expected, pos) :: more =>
          // HINT: You can use the `subst_*` helper above to replace a type variable
          //       by another type in your current set of constraints.
          (found,expected) match {

            case (TypeVariable(id), t2: TypeVariable) =>
              if(id == t2.id) {
                solveConstraints(more)
              } else {
                solveConstraints(subst_*(constraints, id, t2))
              }
            case (tpe, TypeVariable(id)) =>
              solveConstraints(subst_*(constraints,id, tpe))
            case (TypeVariable(id), other) => 
              // Orient
              solveConstraints(Constraint(expected, found, pos) :: more)

            // No type variables
            case (type1, type2) =>
              // Delete useless
              if(type1 == type2) {
                solveConstraints(more)
              }
              else {
                error(s"Type error; expected $expected, found $found", pos)
                solveConstraints(more)
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
