object Pascal
{
  //yes I'm incredibly bad at being original
  def pascal(c: Int, r: Int): Int =
  {
    if(c <= 0 || r <= 0 || r<=c)
    {
      1
    }
    else
    {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  def pascalRowHelper(r: Int, c: Int): String =
  {
    if(r<c)
    {
      ""
    }
    else
    {
      Std.intToString(pascal(c,r)) ++ " " ++ pascalRowHelper(r,c+1)
    }
  }
  def pascalRow(r: Int): String =
  {
    pascalRowHelper(r,0)
  }
  def printPascal(n: Int, acc: Int): Unit =
  {
    Std.printString(pascalRow(acc));
    if(acc<n)
    {
      printPascal(n, acc+1)
    }
    else{()}
  }


//  Std.printString("How many rows of the pascal triangle do you wanna see?");
 // val num: Int = Std.readInt();
  val num: Int = 7;
  printPascal(num, 0);



  val s: String = "hi";
  val str: String = "hi";
  val str2: String = "not hi";
  Std.printString("s==s: " ++ Std.booleanToString(s==s));
  Std.printString("'hi'=='hi': " ++ Std.booleanToString(str==s))
}
