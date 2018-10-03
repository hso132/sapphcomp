package amyc.test

import amyc.parsing._
import org.junit.Test

class LexerTests extends TestSuite {
  val pipeline = Lexer andThen DisplayTokens

  val baseDir = "lexer"

  val outputExt = "txt"

  @Test def testKeywords = shouldOutput("Keywords")
  @Test def testHello = shouldOutput("Hello");
  @Test def testArith = shouldOutput("Arithmetic");
  @Test def badCommentTest = shouldOutput("BadComment");
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

  @Test def testSingleAmp = shouldFail("SingleAmp")

}
