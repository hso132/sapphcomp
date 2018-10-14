package amyc
package parsing

import grammarcomp.grammar.CFGrammar._
import grammarcomp.grammar.GrammarDSL._
import grammarcomp.grammar.GrammarUtils.InLL1
import grammarcomp.grammar._
import grammarcomp.parsing._
import amyc.utils._
import ast.NominalTreeModule._
import Tokens._

// The parser for Amy
// Absorbs tokens from the Lexer and then uses grammarcomp to generate parse trees.
// Defines two different grammars, a naive one which does not obey operator precedence (for demonstration purposes)
// and an LL1 grammar that implements the true syntax of Amy
object Parser extends Pipeline[Stream[Token], Program] {

  /* This grammar does not implement the correct syntax of Amy and is not LL1
   * It is given as an example
   */
  val amyGrammar = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'CaseClassDef | 'FunDef,
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'Params ::= epsilon() | 'Param ~ 'ParamList,
    'ParamList ::= epsilon() | COMMA() ~ 'Param ~ 'ParamList,
    'Param ::= 'Id ~ COLON() ~ 'Type,
    'OptExpr ::= 'Expr | epsilon(),
    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'QName,
    'QName ::= 'Id | 'Id ~ DOT() ~ 'Id,
    'Expr ::= 'Id | 'Literal | 'Expr ~ 'BinOp ~ 'Expr | BANG() ~ 'Expr | MINUS() ~ 'Expr |
              'QName ~ LPAREN() ~ 'Args ~ RPAREN() | 'Expr ~ SEMICOLON() ~ 'Expr |
              VAL() ~ 'Param ~ EQSIGN() ~ 'Expr ~ SEMICOLON() ~ 'Expr |
              IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ LBRACE() ~ 'Expr ~ RBRACE() ~ ELSE() ~ LBRACE() ~ 'Expr ~ RBRACE() |
              'Expr ~ MATCH() ~ LBRACE() ~ 'Cases ~ RBRACE() |
              ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
              LPAREN() ~ 'Expr ~ RPAREN(),
    'Literal ::= TRUE() | FALSE() | LPAREN() ~ RPAREN() | INTLITSENT | STRINGLITSENT,
    'BinOp ::= PLUS() | MINUS() | TIMES() | DIV() | MOD() | LESSTHAN() | LESSEQUALS() |
               AND() | OR() | EQUALS() | CONCAT(),
    'Cases ::= 'Case | 'Case ~ 'Cases,
    'Case ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr,
    'Pattern ::= UNDERSCORE() | 'Literal | 'Id | 'QName ~ LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= epsilon() | 'Pattern ~ 'PatternList,
    'PatternList ::= epsilon() | COMMA() ~ 'Pattern ~ 'PatternList,
    'Args ::= epsilon() | 'Expr ~ 'ExprList,
    'ExprList ::= epsilon() | COMMA() ~ 'Expr ~ 'ExprList,
    'Id ::= IDSENT
  ))

  // TODO: Write a grammar that implements the correct syntax of Amy and is LL1.
  // You can start from the example above and work your way from there.
  // Make sure you use the warning (see `run` below) that tells you which part is not in LL1.
  lazy val amyGrammarLL1 = Grammar('Program, List[Rules[Token]](
    'Program ::= 'ModuleDefs,
    'ModuleDefs ::= 'ModuleDef ~ 'ModuleDefs | epsilon(),
    'ModuleDef ::= OBJECT() ~ 'Id ~ LBRACE() ~ 'Definitions ~ 'OptExpr ~ RBRACE() ~ EOF(),
    'OptExpr ::= 'Expr | epsilon(),
    'Definitions ::= 'Definition ~ 'Definitions | epsilon(),
    'Definition ::= 'AbstractClassDef | 'FunDef | 'CaseClassDef,
    'FunDef ::= DEF() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ COLON() ~ 'Type ~ EQSIGN() ~ LBRACE() ~ 'Expr ~ RBRACE(),
    'AbstractClassDef ::= ABSTRACT() ~ CLASS() ~ 'Id,
    'CaseClassDef ::= CASE() ~ CLASS() ~ 'Id ~ LPAREN() ~ 'Params ~ RPAREN() ~ EXTENDS() ~ 'Id,

    'Expr ::= 'ValAssign ~ SEMICOLON() ~ 'Expr | 'MatchExpr ~ 'ExprConcatList,
    'ExprConcatList ::= SEMICOLON() ~ 'Expr | epsilon(),
    'ValAssign ::= VAL() ~ 'Param ~ EQSIGN() ~ 'MatchExpr,

    'MatchExpr ::= 'OrExpr ~ 'MatchList,
    'MatchList ::= 'Match ~ 'MatchList | epsilon(),
    'OrExpr ::= 'AndExpr ~ 'OrList,
    'OrList ::= OR() ~ 'AndExpr ~ 'OrList | epsilon(),
    'AndExpr ::= 'EqExpr ~ 'AndList,
    'AndList ::= AND() ~ 'EqExpr ~ 'AndList | epsilon(),
    'EqExpr ::= 'CompExpr ~ 'EqList,
    'EqList ::= EQUALS() ~ 'CompExpr ~ 'EqList | epsilon(),
    'CompExpr ::= 'AddExpr ~ 'CompList,
    'CompList ::= LESSEQUALS() ~ 'CompRest | LESSTHAN() ~ 'CompRest | epsilon(),
    'CompRest ::= 'AddExpr ~ 'CompList,

    'AddExpr ::= 'MultExpr ~ 'TermList,
    'TermList ::= PLUS() ~ 'TermRest | MINUS() ~ 'TermRest | CONCAT() ~ 'TermRest | epsilon(),
    'TermRest ::= 'MultExpr ~ 'TermList,

    'MultExpr ::= 'UnaryExpr ~ 'FactorList,
    'FactorList ::= TIMES() ~ 'RestFactor | DIV() ~ 'RestFactor | MOD() ~ 'RestFactor | epsilon(),
    'RestFactor ::= 'UnaryExpr ~ 'FactorList,
    'UnaryExpr ::= 'UnaryOp ~ 'StructExpr,
    'UnaryOp ::= BANG() | MINUS() | epsilon(),
    'StructExpr ::= IF() ~ LPAREN() ~ 'Expr ~ RPAREN() ~ 'BraceExpr ~ ELSE() ~ 'BraceExpr |
                    ERROR() ~ LPAREN() ~ 'Expr ~ RPAREN() |
                    'Id ~ 'OptFunCall |
                    'Literal |
                    LPAREN() ~ 'UnitOrExpr,
    'BraceExpr ::= LBRACE() ~ 'Expr ~ RBRACE(),

    'Match ::= MATCH() ~ LBRACE() ~ 'MatchCase ~ 'MatchCaseList ~ RBRACE(),
    'MatchCase ::= CASE() ~ 'Pattern ~ RARROW() ~ 'Expr, 
    'MatchCaseList ::= 'MatchCase ~ 'MatchCaseList | epsilon(),

    'Params ::= 'Param ~ 'ParamDefList | epsilon(),
    'ParamDefList ::= COMMA() ~ 'Param ~ 'ParamDefList | epsilon(),
    'Param ::= 'Id ~ COLON() ~ 'Type,
    
    'Pattern ::= 'Id ~ 'CaseClassOrId | 'Literal | UNDERSCORE(),
    'CaseClassOrId ::= DOT() ~ 'Id ~ 'ParenPattern | 'ParenPattern | epsilon(),
    'ParenPattern ::= LPAREN() ~ 'Patterns ~ RPAREN(),
    'Patterns ::= 'Pattern ~ 'PatternList | epsilon(),
    'PatternList ::= COMMA() ~ 'Pattern ~ 'PatternList | epsilon(),

    'Type ::= INT() | STRING() | BOOLEAN() | UNIT() | 'Id ~ 'OptId,
    'UnitOrExpr ::= 'Expr ~ RPAREN() | RPAREN(),
    'Literal ::= TRUE() | FALSE() | INTLITSENT | STRINGLITSENT,
    'OptFunCall ::= DOT() ~ 'Id ~ 'FunCall | 'FunCall | epsilon(),
    'FunCall ::= LPAREN() ~ 'Args ~ RPAREN(),
    'Args ::= 'Expr ~ 'ExprList | epsilon(),
    'ExprList ::= COMMA() ~ 'Expr ~ 'ExprList | epsilon(),
    'OptId ::= DOT() ~ 'Id | epsilon(),
    'Id ::= IDSENT
  ))

  def run(ctx: Context)(tokens: Stream[Token]): Program = {
    val (grammar, constructor) = (amyGrammarLL1, new ASTConstructorLL1)
    //val (grammar, constructor) = (amyGrammar, new ASTConstructor)

    import ctx.reporter._
    implicit val gc = new GlobalContext()
    implicit val pc = new ParseContext()

    GrammarUtils.isLL1WithFeedback(grammar) match {
      case InLL1() =>
        // info("Grammar is in LL1")
      case other =>
        warning(other)
    }

    val feedback = ParseTreeUtils.parseWithTrees(grammar, tokens.toList)
    feedback match {
      case s: Success[Token] =>
        constructor.constructProgram(s.parseTrees.head)
      case err@LL1Error(_, Some(tok)) =>
        fatal(s"Parsing failed: $err", tok.obj.position)
      case err =>
        fatal(s"Parsing failed: $err")
    }
  }

}
