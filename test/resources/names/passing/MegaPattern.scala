object MegaPattern
{
  abstract class List
  case class Nil() extends List
  case class Cons(x: Int, xs: List) extends List
  val list: List = Nil();
  list match 
  {
    case Nil() => 0
    case Cons(h,Cons(h0,t)) => -3
    case Cons(Cons(h1,t1), Cons(Cons(h2,t2), t3)) => 5
    case _ => ()
  }
}
