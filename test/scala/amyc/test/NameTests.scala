package amyc.test

import amyc._
import amyc.parsing._
import amyc.utils._
import amyc.ast._
import amyc.analyzer._

import org.junit.Test

class NameTests extends TestSuite  with MainHelpers {

  val pipeline = Lexer andThen
    Parser andThen
    NameAnalyzer andThen
    treePrinterS("Trees after name analysis");

  val baseDir = "names"
  val outputExt = "txt"

  @Test def testArithmetic = shouldOutput("Arithmetic");
  @Test def testAssocTest = shouldOutput("AssocTest");
  @Test def testDivZero = shouldOutput("DivZero");
  @Test def testEqualityTests = shouldOutput("EqualityTests");
  @Test def testFactorial = shouldOutput("Factorial");
  @Test def testHanoi = shouldOutput("Hanoi");
  @Test def testHelloInt = shouldOutput("HelloInt");
  @Test def testHello = shouldOutput("Hello");
  @Test def testMegaPattern = shouldOutput("MegaPattern");
  @Test def testPascal = shouldOutput("Pascal");
  @Test def testPrinting = shouldOutput("Printing");
  @Test def testTestLists = shouldOutput("TestLists");
  @Test def testTreeSet = shouldOutput("TreeSet");

}


