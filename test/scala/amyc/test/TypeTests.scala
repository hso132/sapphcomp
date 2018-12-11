package amyc.test

import amyc._
import amyc.ast._
import amyc.utils._
import amyc.parsing._
import amyc.ast.SymbolicTreeModule._
import amyc.analyzer._

import org.junit.Test

object NoPrint extends Pipeline[(SymbolicTreeModule.Program, SymbolTable),Unit] {
  def run(ctx: Context)(v: (Program, SymbolTable)): Unit = {
  }
}
class TypeTests extends TestSuite {
  val pipeline = 
    Lexer andThen
  Parser andThen
  NameAnalyzer andThen
  TypeChecker andThen
  NoPrint;
  val baseDir = "types"
  val outputExt = "txt"

  val libs: List[String] = "Std" :: "Option" :: "List" :: Nil;
  @Test def testArithmetic = shouldPass(libs :+ "Arithmetic", "Arithmetic");
  @Test def testAssocTest = shouldPass(libs :+ "AssocTest", "AssocTest");
  @Test def testDivZero = shouldFail(libs :+ "DivZero", "DivZero");
  @Test def testEqualityTests = shouldPass(libs :+ "EqualityTests", "EqualityTests");
  @Test def testFactorial = shouldPass(libs :+ "Factorial", "Factorial");
  @Test def testHanoi = shouldPass(libs :+ "Hanoi", "Hanoi");
  @Test def testHelloInt = shouldPass(libs :+ "HelloInt", "HelloInt");
  @Test def testHello = shouldPass(libs :+ "Hello", "Hello");
  @Test def testMegaPattern = shouldFail(libs :+ "MegaPattern", "MegaPattern");
  @Test def testName = shouldFail(libs :+ "Name", "Name");
  @Test def testPascal = shouldPass(libs :+ "Pascal", "Pascal");
  @Test def testPrinting = shouldPass(libs :+ "Printing", "Printing");
  @Test def testTestLists = shouldPass(libs :+ "TestLists", "TestLists");
  @Test def testTreeSet = shouldPass(libs :+ "TreeSet", "TreeSet");
  @Test def test1 = shouldFail("()true")
  @Test def test2 = shouldFail("()H()")
  @Test def test3 = shouldFail("()3")
  @Test def test4 = shouldFail("true()")
  @Test def test6 = shouldFail("trueH()")
  @Test def test7 = shouldFail("true3")
  @Test def test8 = shouldFail("H()()")
  @Test def test9 = shouldFail("H()true")
  @Test def test11 = shouldFail("H()3")
  @Test def test12 = shouldFail("3()")
  @Test def test13 = shouldFail("3true")
  @Test def test14 = shouldFail("3H()")

}
