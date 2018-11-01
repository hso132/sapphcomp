package amyc
package parsing

import grammarcomp.parsing._
import utils.Positioned
import ast.NominalTreeModule._
import Tokens._

// Implements the translation from parse trees to ASTs for the LL1 grammar,
// that is, this should correspond to Parser.amyGrammarLL1.
// We extend the plain ASTConstructor as some things will be the same -- you should
// override whatever has changed. You can look into ASTConstructor as an example.
class ASTConstructorLL1 extends ASTConstructor {

  //checks if the given tree is just the empty string
  def splitExprList(pTree: NodeOrLeaf[Token]): Option[List[NodeOrLeaf[Token]]] = {
    (pTree: @unchecked) match {
      case Node( _ ::= List(), List()) => None
      case Node( _ ::= _, l) => Some(l)
    }
  }

  //builds an expression from a binary split (left: operand, right: operator and operand)
  def exprListToExpr(left: NodeOrLeaf[Token], right: NodeOrLeaf[Token]): Expr = {
    val expr1 = constructExpr(left);
    constructOpExpr(expr1, right);
  }


  //override to add the case where we pass a leaf directly
  override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr = {
    ptree match {
      case Node(_, List(Leaf(t))) => tokenToExpr(t)
      case Leaf(t) => tokenToExpr(t)
    }
  }

  //constructs a sequence tree
  def constructSequence(l: Expr, e2: Expr, r: NodeOrLeaf[Token]): Sequence = {
    val rhs = splitExprList(r);
    rhs match {
      case None => Sequence(l, e2).setPos(l)
      case Some(List(_,next_e2, next_el)) =>
        Sequence(l, constructSequence(l, constructExpr(next_e2),next_el))
    }
  }

  def constructLet(pTree: NodeOrLeaf[Token], body: Expr): Let = {
    // The non-terminal only has one child
    val Node('ValAssign ::= _, List(_,paramDef,_, value)) = pTree;
    Let(constructParam(paramDef), constructExpr(value),body)
  }

  def constructMatch(scrut: Expr, pTree: NodeOrLeaf[Token]): Match = {
    // Non-terminal only has one child
    val Node('Match ::= _, List(_, _, matchCase, rest, _)) = pTree;
    Match(scrut, constructMatchCaseList(matchCase, rest)).setPos(scrut);
  }

  def constructMatchCaseList(matchCase: NodeOrLeaf[Token], 
    rest: NodeOrLeaf[Token]): List[MatchCase] = {
      val Node('MatchCase ::=  CASE() :: _ , List(_, pat, _, ex)) = matchCase;
      val currPat = constructPattern(pat);
      val currExpr = constructExpr(ex);
      val currMatchCase = MatchCase(currPat,currExpr);
      rest match {
        case Node(_ ::= Nil, Nil) => currMatchCase :: Nil;
        case Node(_ ::= 'MatchCase :: _, List(newMatchCase, newRest)) =>
          currMatchCase :: constructMatchCaseList(newMatchCase, newRest)
      }
  }

override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern = {
  pTree match {
    // Wildcard
    case Node(_ ::= UNDERSCORE()::Nil, List(Leaf(posMarker))) => 
      WildcardPattern().setPos(posMarker)
      // Literal
    case Node(_ ::= List('Literal), List(lit)) => LiteralPattern(constructLiteral(lit))
    // Unit Literal
    case Node(_ ::= List(LPAREN(),RPAREN()), List(l,_)) => 
      LiteralPattern(UnitLiteral())

      // Stuff that starts with an ID
    case Node(_ ::= 'Id :: _, List(id1, restPatternOrQname)) =>
      restPatternOrQname match {
        // Fully qualified name => function call
    case Node(_ ::= DOT() :: _, List(_, id2, parenPattern)) => 
      val id1Str = extractId(id1);
      val id2Str = extractId(id2);
      val qName = QualifiedName(Some(id1Str), id2Str);
      val Node('ParenPattern ::= _, List(_, patterns,_)) = parenPattern;
      val patternList: List[Pattern] = constructPatterns(patterns);
      CaseClassPattern(qName, patternList)
      // Function call only
    case Node(_ ::= List('ParenPattern), List(parenPattern)) =>
      val qName = QualifiedName(None, extractId(id1));
      val Node('ParenPattern ::= _, List(_, patterns,_)) = parenPattern;
      val patternList: List[Pattern] = constructPatterns(patterns);
      CaseClassPattern(qName, patternList)
      // Nothing => was just an ID
    case Node(_ ::= List(), List()) => IdPattern(extractId(id1))
      }
  }
}

// Constructs a list of patterns
def constructPatterns(pTree: NodeOrLeaf[Token]): List[Pattern] = {
  pTree match {
    case Node('Patterns ::= List(), List()) => Nil;
    case Node('Patterns ::= 'Pattern :: _, List(pat, rest)) => 
      constructPattern(pat) :: constructPatterns(rest);
    case Node('PatternList ::= List(), List()) => Nil;
    case Node('PatternList ::= COMMA() :: _, List(_, pat, rest)) =>
      constructPattern(pat) :: constructPatterns(rest);
  }
}

// Constructs a type
override def constructType(pTree: NodeOrLeaf[Token]): TypeTree = {
  pTree match {
    case Node('Type ::= _, List(Leaf(tp))) =>
      super.constructType(pTree)
    case Node('Type ::= _, List(id,optid)) =>
      val id1 = extractId(id);
      optid match
      {
        case Node('OptId ::= List(),List()) => 
          TypeTree(ClassType(QualifiedName(None, id1)))
        case Node('OptId ::= List(DOT(), 'Id), List(_, extendId)) =>
          val id2 = extractId(extendId);
          TypeTree(ClassType(QualifiedName(Some(id1),id2)))
      }
  }
}

override def constructExpr(pTree: NodeOrLeaf[Token]): Expr = {
  pTree match {

    case Node('Expr ::= _, List(matchExpr, exprConcatList)) => 
      val expr1 = constructExpr(matchExpr);
      val rhs = splitExprList(exprConcatList)
      rhs match {
        case None => expr1
        case Some(List(_,e2)) => Sequence(expr1, constructExpr(e2)).setPos(expr1)
      }
    case Node('Expr ::= 'ValAssign :: _, List(valAssign, _, body)) =>
      constructLet(valAssign,constructExpr(body))

    case Node('MatchExpr ::= _, List(e1, el)) =>
      val expr1 = constructExpr(e1);
      (el: @unchecked) match {
        case Node('MatchList ::= List(), List()) => expr1
        case Node('MatchList ::= List('Match), List(matcher)) =>
          constructMatch(expr1,matcher)
      }

    case Node('UnaryExpr ::= _, List(e1@Node(_,_),el)) => {
      lazy val expr = constructExpr(el);
      e1 match {
        // No operator
        case Node( _ ::= List(), List()) =>expr
        // An operator is present
        case Node( _ ::= List(_), List(Leaf(p))) =>
        (p: @unchecked) match {
          case BANG() => Not(expr).setPos(p)
          case MINUS() => Neg(expr).setPos(p)
        }
      };
    }

    // If Statement
    case Node('StructExpr::= List(IF(), LPAREN(), 'Expr, RPAREN(), 'BraceExpr, ELSE(), 'BraceExpr), List(Leaf(pt),_,e1,_,e2,_,e3)) =>
      val expr1 = constructExpr(e1);
      val expr2 = constructExpr(e2);
      val expr3 = constructExpr(e3);
      Ite(expr1,expr2,expr3).setPos(pt)

    // Literal
    case Node('StructExpr ::= List('Literal), List(litTree)) =>
      constructLiteral(litTree)
    // Error 
    case Node('StructExpr ::= ERROR() :: _, List(Leaf(err),_,e1,_)) =>
      Error(constructExpr(e1)).setPos(err)
    // Function call or variable
    case Node('StructExpr::= List('Id, 'OptFunCall), List(id1,optfuncall)) => {
      val id1Str = extractId(id1);
      val posIndicator = extractPosIndicator(id1)

      optfuncall match {
        // Variable
        case Node(_ ::= List(), List()) => Variable(id1Str).setPos(posIndicator)
        // Fully qualified Name => function call
        case Node(_ ::= DOT() :: _, List(_,id2, funcall)) => 
          val id2Str = extractId(id2);
          val args = getFunCallArgs(funcall);
          Call(QualifiedName(Some(id1Str),id2Str),args).setPos(posIndicator)
        // Function call
        case Node(_ ::= List('FunCall), List(funcall)) =>
      val args = getFunCallArgs(funcall);
      Call(QualifiedName(None, id1Str),args).setPos(posIndicator)
      }
    }

    // Either unit, or an expression between brackets
    case Node('StructExpr ::= LPAREN() :: _, List(Leaf(lparen), unitOrParenExpr)) =>
      unitOrParenExpr match {
        case Node(_ ::= List(RPAREN()), List(_)) => UnitLiteral().setPos(lparen);
        case Node(_ ::= 'Expr :: _, List(e1,_)) => constructExpr(e1);
      }
    // An expression beween braces
    case Node('BraceExpr ::= List(LBRACE(), 'Expr, RBRACE()), List(_,e1,_)) =>
      constructExpr(e1)

    //binary operations
    case Node( sym ::= _, List(e1,el)) =>
      exprListToExpr(e1,el)
  }
}

def extractPosIndicator(ptree: NodeOrLeaf[Token]): Positioned = {
  ptree match {
    case Node(_, List(Leaf(posIndicator))) => posIndicator
    case Leaf(posIndicator) => posIndicator
  }
}

// Extract ID from singleton Node
def extractId(n: NodeOrLeaf[Token]): String = {
  val Node(_,List(Leaf(ID(id1)))) = n;
  id1;
}

def getFunCallArgs(funcall: NodeOrLeaf[Token]): List[Expr] = {
  // Isolate arguments into their own tree
  val Node(_, List(_, args, _)) = funcall;
  getArgs(args)
}

// Translate argument tree into list of args
def getArgs(argsPTree: NodeOrLeaf[Token]): List[Expr] = {
  argsPTree match {
    // No arguments
    case Node('Args ::= List(),List()) => List()
    // Begining of list
    case Node('Args ::= List('Expr, 'ExprList), List(e1,el)) => constructExpr(e1) :: getArgs(el)
    // Mid section of list
    case Node('ExprList ::= List(COMMA(), 'Expr, 'ExprList), List(_, e1, el)) =>
      constructExpr(e1) :: getArgs(el)
    // End of list
    case Node('ExprList ::= List(), List()) => List()
  }
}
// Important helper method:
// Because LL1 grammar is not helpful in implementing left associativity,
// we give you this method to reconstruct it.
// This method takes the left operand of an operator (leftopd)
// as well as the tree that corresponds to the operator plus the right operand (ptree)
// It parses the right hand side and then reconstruct the operator expression
// with correct associativity.
// If ptree is empty, it means we have no more operators and the leftopd is returned.
// Note: You may have to override constructOp also, depending on your implementation
def constructOpExpr(leftopd: Expr, ptree: NodeOrLeaf[Token]): Expr = {
  ptree match {
    case Node(_, List()) => //epsilon rule of the nonterminals
      leftopd
    case Node(sym ::= _, List(op, rightNode))
    if Set('OrList, 'AndList, 'EqList, 'CompList, 'TermList, 'FactorList) contains sym =>
      rightNode match {
        case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
          val nextAtom = constructExpr(nextOpd)
          constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
      }
  }
}

}

