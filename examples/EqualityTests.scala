object testslol
{
  def assert(test: Boolean, test_name: String): Unit =
  {
    if(!test)
    {
      error("test failed " ++ test_name)
    }
    else
    {
      Std.printString("test succeeded: " ++ test_name)
    }
  }
  abstract class hi
  case class Nil() extends hi

  assert(0 == 0, "zero equals zero");
  assert(1+2 == 3, "one plus two equals three");
  assert(() == (), "unit equals unit");
  assert(true == true, "true equals true");
  assert(false == false, "false equals false");
  assert(!("hi" == "hi"), "string does not equal other string");
  assert(!(Nil() == Nil()), "Nil does not equal nil");
  
  ()
}
