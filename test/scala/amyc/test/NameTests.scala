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
  val libs: List[String] = "Std" :: "Option" :: "List" :: Nil;         
  @Test def testArithmetic = shouldOutput("Arithmetic" :: libs, "Arithmetic");           
  @Test def testAssocTest = shouldOutput("AssocTest" :: libs, "AssocTest");              
  @Test def testDivZero = shouldOutput("DivZero" :: libs, "DivZero");                    
  @Test def testEqualityTests = shouldOutput("EqualityTests" :: libs, "EqualityTests");  
  @Test def testFactorial = shouldOutput("Factorial" :: libs, "Factorial");              
  @Test def testHanoi = shouldOutput("Hanoi" :: libs, "Hanoi");                          
  @Test def testHelloInt = shouldOutput("HelloInt" :: libs, "HelloInt");                 
  @Test def testHello = shouldOutput("Hello" :: libs, "Hello");                          
  @Test def testMegaPattern = shouldOutput("MegaPattern" :: libs, "MegaPattern");        
  @Test def testName = shouldOutput("Name" :: libs, "Name");                             
  @Test def testPascal = shouldOutput("Pascal" :: libs, "Pascal");                       
  @Test def testPrinting = shouldOutput("Printing" :: libs, "Printing");                 
  @Test def testTestLists = shouldOutput("TestLists" :: libs, "TestLists");              
  @Test def testTreeSet = shouldOutput("TreeSet" :: libs, "TreeSet");                    
}


