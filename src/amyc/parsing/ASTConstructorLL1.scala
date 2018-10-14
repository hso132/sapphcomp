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

   val rhs = splitExprList(right);
   val expr1 = constructExpr(left);
   rhs match
   {
     case None => expr1;
     case Some(List(op, e2)) => 
       val expr2 = constructExpr(e2);
       constructOp(op)(expr1,expr2).setPos(expr1);
   }
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
         case Node('MatchList ::= List('Match,'MatchList), List(matcher, matchList)) => ???
       }

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
    
     //literal
     case Node('StructExpr::= List('Literal), List(litTree)) => constructLiteral(litTree)
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
       }
     }
     //binary operations
     case Node( sym ::= _, List(e1,el)) =>
       exprListToExpr(e1,el)


     //better stack trace
     case Node(l1,l2) =>
       println(l1);
       println(l2);
       throw new scala.MatchError

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
     if Set('OrExpr, 'AndExpr, 'EqExpr, 'CompExpr, 'AddExpr, 'MultExpr) contains sym =>
       rightNode match {
         case Node(_, List(nextOpd, suf)) => // 'Expr? ::= Expr? ~ 'OpExpr,
           val nextAtom = constructExpr(nextOpd)
           constructOpExpr(constructOp(op)(leftopd, nextAtom).setPos(leftopd), suf) // captures left associativity
       }
   }
 }

}

