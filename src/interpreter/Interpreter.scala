package amyc
package interpreter

import utils._
import ast.SymbolicTreeModule._
import ast.Identifier
import analyzer.SymbolTable

// An interpreter for Amy programs, implemented in Scala
object Interpreter extends Pipeline[(Program, SymbolTable), Unit] {

  // A class that represents a value computed by interpreting an expression
  abstract class Value {
    def asInt: Int = this.asInstanceOf[IntValue].i
    def asBoolean: Boolean = this.asInstanceOf[BooleanValue].b
    def asString: String = this.asInstanceOf[StringValue].s

    override def toString: String = this match {
      case IntValue(i) => i.toString
      case BooleanValue(b) => b.toString
      case StringValue(s) => s
      case UnitValue => "()"
      case CaseClassValue(constructor, args) =>
        constructor.name + "(" + args.map(_.toString).mkString(", ") + ")"
    }
  }
  case class IntValue(i: Int) extends Value
  case class BooleanValue(b: Boolean) extends Value
  case class StringValue(s: String) extends Value
  case object UnitValue extends Value
  case class CaseClassValue(constructor: Identifier, args: List[Value]) extends Value

  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
    val (program, table) = v

    // These built-in functions do not have an Amy implementation in the program,
    // instead their implementation is encoded in this map
    val builtIns: Map[(String, String), (List[Value]) => Value] = Map(
      ("Std", "printInt")    -> { args => println(args.head.asInt); UnitValue },
      ("Std", "printString") -> { args => println(args.head.asString); UnitValue },
      ("Std", "readString")  -> { args => StringValue(scala.io.StdIn.readLine()) },
      ("Std", "readInt")     -> { args =>
        val input = scala.io.StdIn.readLine()
        try {
          IntValue(input.toInt)
        } catch {
          case ne: NumberFormatException =>
            ctx.reporter.fatal(s"""Could not parse "$input" to Int""")
        }
      },
      ("Std", "intToString")   -> { args => StringValue(args.head.asInt.toString) },
      ("Std", "digitToString") -> { args => StringValue(args.head.asInt.toString) }
    )

    // Utility functions to interface with the symbol table.
    def isConstructor(name: Identifier) = table.getConstructor(name).isDefined
    def findFunctionOwner(functionName: Identifier) = table.getFunction(functionName).get.owner.name
    def findFunction(owner: String, name: String) = {
      program.modules.find(_.name.name == owner).get.defs.collectFirst {
        case fd@FunDef(fn, _, _, _) if fn.name == name => fd
      }.get
    }

    // Interprets a function, using evaluations for local variables contained in 'locals'
    // TODO: Complete all missing cases. Look at the given ones for guidance.
    def interpret(expr: Expr)(implicit locals: Map[Identifier, Value]): Value = {
      def getAndCheckForZero(lhs: Expr, rhs: Expr): (Int, Int) = {
        val a: Int = interpret(lhs).asInt
        val b: Int = interpret(rhs).asInt
        if(b==0) {
          ctx.reporter.fatal("Division by zero!")
        }
        else {
          (a,b)
        }
      }
      expr match {
        case Variable(name) =>
          locals(name)
        case IntLiteral(i) =>
          IntValue(i)
        case BooleanLiteral(b) =>
          BooleanValue(b)
        case StringLiteral(s) =>
          StringValue(s)
        case UnitLiteral() =>
          UnitValue
        case Plus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt + interpret(rhs).asInt)
        case Minus(lhs, rhs) =>
          IntValue(interpret(lhs).asInt - interpret(rhs).asInt)
        case Times(lhs, rhs) =>
          IntValue(interpret(lhs).asInt * interpret(rhs).asInt)
        case Div(lhs, rhs) =>
          val (a,b) = getAndCheckForZero(lhs,rhs)
          IntValue(a/b)
        case Mod(lhs, rhs) =>
          val (a,b) = getAndCheckForZero(lhs,rhs)
          IntValue(a%b)
        case LessThan(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt < interpret(rhs).asInt)
        case LessEquals(lhs, rhs) =>
          BooleanValue(interpret(lhs).asInt <= interpret(rhs).asInt)
        case And(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean && interpret(rhs).asBoolean)
        case Or(lhs, rhs) =>
          BooleanValue(interpret(lhs).asBoolean || interpret(rhs).asBoolean)
        case Equals(lhs, rhs) =>
          val interpretedLhs = interpret(lhs)
          val interpretedRhs = interpret(rhs)
          val compRes = (interpretedLhs, interpretedRhs) match {
            case (IntValue(i1), IntValue(i2)) => (i1 == i2)
            case (BooleanValue(b1), BooleanValue(b2)) => (b1==b2)
            case (StringValue(_), StringValue(_)) => interpretedLhs eq interpretedRhs
            case (CaseClassValue(_, _), CaseClassValue(_, _)) => 
              interpretedLhs eq interpretedRhs
            case (UnitValue, UnitValue) => true
            // Should never happen
            case other => throw new scala.MatchError(other)
          }
          BooleanValue(compRes)
        case Concat(lhs, rhs) =>
          StringValue(interpret(lhs).asString + interpret(rhs).asString)
        case Not(e) =>
          BooleanValue(!interpret(e).asBoolean)
        case Neg(e) =>
          IntValue(-interpret(e).asInt)
        case Call(qname, args) =>
          //       then if it is a built-in function (otherwise it is a normal function).
          //       Use the helper methods provided above to retrieve information from the symbol table.
          //       Think how locals should be modified.
          val interpretedArgs = args map interpret
          if(isConstructor(qname)) {
            CaseClassValue(qname, interpretedArgs)
          } else {
            val owner = findFunctionOwner(qname)
            val name = qname.name
            if(builtIns.contains((owner,name))) {
              val fct = builtIns((owner,name))
              fct(interpretedArgs)
            } else {
              val funDef: FunDef = findFunction(owner,name)
              val paramNames: List[Name] = funDef.params.map(_.name)
              interpret(funDef.body)((paramNames zip interpretedArgs).toMap)
            }
          }
        case Sequence(e1, e2) =>
          interpret(e1);
          interpret(e2)
        case Let(df, value, body) =>
          val assignedValue = interpret(value)
          interpret(body)(locals+(df.name -> assignedValue))
        case Ite(cond, thenn, elze) =>
          val condResult = interpret(cond).asBoolean
          if(condResult) {
            interpret(thenn)
          } else {
            interpret(elze)
          }
        case Match(scrut, cases) =>
          // Hint: We give you a skeleton to implement pattern matching
          //       and the main body of the implementation

          val evS = interpret(scrut)

          // Returns a list of pairs id -> value,
          // where id has been bound to value within the pattern.
          // Returns None when the pattern fails to match.
          // Note: Only works on well typed patterns (which have been ensured by the type checker).
          def matchesPattern(v: Value, pat: Pattern): Option[List[(Identifier, Value)]] = {
            ((v, pat): @unchecked) match {
              case (_, WildcardPattern()) =>
                Option(List())
              case (_, IdPattern(name)) =>
                Some(List(name -> v))
              case (IntValue(i1), LiteralPattern(IntLiteral(i2))) =>
                if(i1==i2) {
                  Some(List())
                } else {
                  None
                }
              case (BooleanValue(b1), LiteralPattern(BooleanLiteral(b2))) =>
                if(b1==b2) {
                  Some(List())
                } else {
                  None
                }
              case (StringValue(_), LiteralPattern(StringLiteral(_))) =>
                None
              case (UnitValue, LiteralPattern(UnitLiteral())) =>
                Some(List())
              case (CaseClassValue(con1, realArgs), CaseClassPattern(con2, formalArgs)) =>
              // Check that the constructors match
              if(con1 == con2) {
                val foundArgs = (realArgs zip formalArgs).map({
                  case (value, pattern) => matchesPattern(value, pattern) })
                  .filter(_.isDefined).map(_.get)
                if(foundArgs.length < formalArgs.length) {
                  None
                } else {
                  Some(foundArgs.flatten)
                }
              } else {
                None
              }
            }
          }

          // Main "loop" of the implementation: Go through every case,
          // check if the pattern matches, and if so return the evaluation of the case expression
          for {
             MatchCase(pat, rhs) <- cases
            moreLocals <- matchesPattern(evS, pat)
          } {
            return interpret(rhs)(locals ++ moreLocals)
          }
          // No case matched: The program fails with a match error
          ctx.reporter.fatal(s"Match error: ${evS.toString}@${scrut.position}")

        case Error(msg) =>
          ctx.reporter.fatal(msg)
      }
    }

    // Body of the interpreter: Go through every module in order
    // and evaluate its expression if present
    for {
      m <- program.modules
      e <- m.optExpr
    } {
      interpret(e)(Map())
    }
  }
}
