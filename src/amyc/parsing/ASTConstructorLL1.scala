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

  // TODO: Override methods from ASTConstructor as needed

  /* ... */


 //checks if the given tree is just the empty string
 def splitExprList(pTree: NodeOrLeaf[Token]): Option[List[NodeOrLeaf[Token]]] =
 {
   (pTree: @unchecked) match
   {
     case Node( _ ::= List(), List()) => None
     case Node( _ ::= _, l) => Some(l)
   }
 }
 def exprListToExpr(left: NodeOrLeaf[Token], right: NodeOrLeaf[Token]): Expr = 
 {
   val expr1 = constructExpr(left);
   constructOpExpr(expr1, right);
 }


 override def constructOp(ptree: NodeOrLeaf[Token]): (Expr, Expr) => Expr =
 {
   ptree match
   {
     case Node(_, List(Leaf(t))) => tokenToExpr(t)
     case Leaf(t) => tokenToExpr(t)
   }
 }
 def constructSequence(l: Expr, e2: Expr, r: NodeOrLeaf[Token]): Sequence =
 {
   val rhs = splitExprList(r);
   rhs match
   {
     case None => Sequence(l, e2).setPos(l)
     case Some(List(_,next_e2, next_el)) => Sequence(l, constructSequence(l, constructExpr(next_e2),next_el))
   }
 }

 def constructLet(pTree: NodeOrLeaf[Token], body: Expr): Let =
 {
   pTree match
   {
     case Node('ValAssign ::= _, List(_,paramDef,_, value)) => Let(constructParam(paramDef), constructExpr(value),body)
   }
 }
 def constructMatch(scrut: Expr, pTree: NodeOrLeaf[Token]): Match = 
 {
   pTree match 
   {
     case Node('Match ::= _, List(_, _, matchCase, rest, _)) => Match(scrut, constructMatchCaseList(matchCase, rest)).setPos(scrut);
   }
 }
def constructMatchCaseList(matchCase: NodeOrLeaf[Token], rest: NodeOrLeaf[Token]): List[MatchCase] = 
{
  val Node('MatchCase ::= List(CASE(), 'Pattern, RARROW(), 'Expr), List(_,pat,_,ex)) = matchCase;

  val currPat = constructPattern(pat);
  val currExpr = constructExpr(ex);
  val currMatchCase = MatchCase(currPat,currExpr)
  rest match
  {
    case Node(_ ::= List(), List()) => currMatchCase :: Nil;
    case Node(_ ::= 'MatchCase :: _, List(newMatchCase, newRest)) => currMatchCase :: constructMatchCaseList(newMatchCase, newRest)
  }
}

override def constructPattern(pTree: NodeOrLeaf[Token]): Pattern =
{
  pTree match
  {
    case Node(_ ::= UNDERSCORE()::Nil, List(Leaf(posMarker))) => 
      WildcardPattern().setPos(posMarker)
    case Node(_ ::= List('Literal), List(lit)) => LiteralPattern(constructLiteral(lit))
    case Node(_ ::= List(LPAREN(),RPAREN()), List(l,_)) => 
      LiteralPattern(UnitLiteral())
    case Node(_ ::= 'Id :: _, List(id1, restPatternOrQname)) =>
      restPatternOrQname match 
      {
        case Node(_ ::= DOT() :: _, List(_, id2, parenPattern)) => 
          val id1Str = extractId(id1);
          val id2Str = extractId(id2);
          val qName = QualifiedName(Some(id1Str), id2Str);
          val Node('ParenPattern ::= _, List(_, patterns,_)) = parenPattern;
          val patternList: List[Pattern] = constructPatterns(patterns);
          CaseClassPattern(qName, patternList)
        case Node(_ ::= List('ParenPattern), List(parenPattern)) =>
          val qName = QualifiedName(None, extractId(id1));
          val Node('ParenPattern ::= _, List(_, patterns,_)) = parenPattern;
          val patternList: List[Pattern] = constructPatterns(patterns);
          CaseClassPattern(qName, patternList)
        case Node(_ ::= List(), List()) => IdPattern(extractId(id1))
      }
  }
}

def constructPatterns(pTree: NodeOrLeaf[Token]): List[Pattern] =
{
  pTree match 
  {
    case Node('Patterns ::= List(), List()) => Nil;
    case Node('Patterns ::= 'Pattern :: _, List(pat, rest)) => 
      constructPattern(pat) :: constructPatterns(rest);
    case Node('PatternList ::= List(), List()) => Nil;
    case Node('PatternList ::= COMMA() :: _, List(_, pat, rest)) =>
      constructPattern(pat) :: constructPatterns(rest);
  }
}

 override def constructType(pTree: NodeOrLeaf[Token]): TypeTree = 
 {
   pTree match
   {
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
 override def constructExpr(pTree: NodeOrLeaf[Token]): Expr = 
 {
   pTree match
   {
     case Node('Expr ::= _, List(mExpr, exprConcatList)) => 
       val expr1 = constructExpr(mExpr);
       val rhs = splitExprList(exprConcatList)
       rhs match
       {
         case None => expr1
         case Some(List(_,e2)) => Sequence(expr1, constructExpr(e2))
       }

    
       
     case Node('Expr ::= 'ValAssign :: _, List(valAssign, _, body)) =>
       constructLet(valAssign,constructExpr(body))

     case Node('MatchExpr ::= _, List(e1, el)) =>
       val expr1 = constructExpr(e1);
       (el: @unchecked) match
       {
         case Node('MatchList ::= List(), List()) => expr1
         case Node('MatchList ::= List('Match), List(matcher)) =>
           constructMatch(expr1,matcher)
       }

     case Node('UnaryExpr ::= _, List(e1@Node(_,_),el)) =>
     {
       lazy val expr = constructExpr(el);
       e1 match
       {
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

     case Node('StructExpr::= List(IF(), LPAREN(), 'Expr, RPAREN(), 'BraceExpr, ELSE(), 'BraceExpr), List(Leaf(pt),_,e1,_,e2,_,e3)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(e2);
       val expr3 = constructExpr(e3);
       Ite(expr1,expr2,expr3).setPos(pt)
    
     //literal
     case Node('StructExpr::= List('Literal), List(litTree)) =>
       constructLiteral(litTree)
     case Node('StructExpr::= ERROR() :: _, List(Leaf(err),_,e1,_)) =>
       Error(constructExpr(e1)).setPos(err)
     //function call or variable
     case Node('StructExpr::= List('Id, 'OptFunCall), List(nid1,optfuncall)) => 
     {
       val id1 = extractId(nid1);

       optfuncall match
       {
         //variable
         case Node(_ ::= List(), List()) => Variable(id1)
         case Node(_ ::= DOT() :: _, List(_,nid2, funcall)) => 
           val id2 = extractId(nid2);
           val args = getFunCallArgs(funcall);
           Call(QualifiedName(Some(id1),id2),args)
         case Node(_ ::= List('FunCall), List(funcall)) =>
             val args = getFunCallArgs(funcall);
             Call(QualifiedName(None, id1),args)
       }
     }
     case Node('StructExpr ::= LPAREN() :: _, List(Leaf(lparen), unitOrParenExpr)) =>
       unitOrParenExpr match {
         case Node(_ ::= List(RPAREN()), List(_)) => UnitLiteral().setPos(lparen);
         case Node(_ ::= 'Expr :: _, List(e1,_)) => constructExpr(e1);
       }
     case Node('BraceExpr ::= List(LBRACE(), 'Expr, RBRACE()), List(_,e1,_)) =>
       constructExpr(e1)

     //binary operations
     case Node( sym ::= _, List(e1,el)) =>
       exprListToExpr(e1,el)
   }
 }

 def extractId(n: NodeOrLeaf[Token]): String =
 {
   val Node(_,List(Leaf(ID(id1)))) = n;
   id1;
 }

 def getFunCallArgs(funcall: NodeOrLeaf[Token]): List[Expr] =
 {
   val Node(_, List(_, args, _)) = funcall;
   getArgs(args)
 }
 def getArgs(argsPTree: NodeOrLeaf[Token]): List[Expr] = 
 {
   argsPTree match
   {
     //no arguments
     case Node('Args ::= List(),List()) => List()
     //begining of list
     case Node('Args ::= List('Expr, 'ExprList), List(e1,el)) => constructExpr(e1) :: getArgs(el)
     //mid section of list
     case Node('ExprList ::= List(COMMA(), 'Expr, 'ExprList), List(_, e1, el)) => constructExpr(e1) :: getArgs(el)
     //end of list
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

