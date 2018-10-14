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

 override def constructExpr(pTree: NodeOrLeaf[Token]): Expr = 
 {
   pTree match
   {
     case Node('Expr ::= _, List(mExpr, exprConcatList)) => 
       val expr1 = constructExpr(mExpr);
       val expr2 = constructExpr(exprConcatList);
       Sequence(expr1,expr2).setPos(expr1)

     case Node('ExprConcatList ::= _,  List(e1,_, e2)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(e2);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('MatchExprOrValAssign ::= _, List(ex)) =>
       constructExpr(ex);
       
     case Node('MatchExpr ::= _, List(e1, el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('OrExpr ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('AndExpr ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('EqExpr ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('CompExpr ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('Sum ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('Term ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node('UnaryExpr ::= _, List(e1@Node(_,_),el)) =>
     {
       lazy val expr = constructExpr(el);
       e1 match
       {
         case Node( _ ::= List(), List()) =>expr
         case Node( _ ::= List(_), List(Leaf(p))) =>expr.setPos(p)
       };
     }

     case Node('StructExpr::= List(IF(), LPAREN(), 'Expr, RPAREN(), 'BraceExpr, ELSE(), 'BraceExpr), List(Leaf(pt),_,e1,_,e2,_,e3)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(e2);
       val expr3 = constructExpr(e3);
       Ite(expr1,expr2,expr3).setPos(pt)
    
     case Node('StructExpr::= List('Literal), List(litTree)) => constructLiteral(litTree)

     case Node('Sum ::= _, List(e1,el)) =>
       val expr1 = constructExpr(e1);
       val expr2 = constructExpr(el);
       Sequence(expr1,expr2).setPos(expr1);

     case Node(l1,l2) =>
       println(l1);
       println(l2);
       throw new scala.MatchError

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
     if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
       rightNode match {
         case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
           val nextAtom = constructExpr(nextOpd)
           constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
       }
   }
 }

}

