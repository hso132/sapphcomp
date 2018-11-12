package amyc.test

import amyc._
import amyc.parsing._
import amyc.utils._
import amyc.ast._
import org.junit.Test

class ParserTests extends TestSuite with MainHelpers {
  val p0 = Lexer andThen Parser andThen treePrinterN("Trees after parsing");
  val pipeline = p0;
  val baseDir = "parser"

  val outputExt = "txt"

  @Test def testHello = shouldOutput("Hello");
  @Test def testArith = shouldOutput("Arithmetic");
  @Test def test_DivZero = shouldOutput("DivZero")
  @Test def test_EqualityTests = shouldOutput("EqualityTests")
  @Test def test_Factorial = shouldOutput("Factorial")
  @Test def test_Hanoi = shouldOutput("Hanoi")
  @Test def test_HelloInt = shouldOutput("HelloInt")
  @Test def test_Hello = shouldOutput("Hello")
  @Test def test_Pascal = shouldOutput("Pascal")
  @Test def test_Printing = shouldOutput("Printing")
  @Test def test_TestLists = shouldOutput("TestLists")
  @Test def test_TreeSet = shouldOutput("TreeSet")
}
