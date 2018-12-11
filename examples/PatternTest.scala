object PatternTest {
  abstract class List
  case class Cons(h: Int, t: List) extends List
  case class Nil() extends List
  def listToString(l: List): String = {
    l match {
      //case Cons(h,t) => Std.intToString(h) ++ ", " ++ listToString(t)
      case Nil() => ""
    }
  }

  val list: List = Cons(5, Nil());
  Std.printString(listToString(list));
  ()
}
