object MegaPattern
{
  list match 
  {
    case Nil() => 0
    case Cons(Cons(h1,t1), Cons(Cons(h2,t2), t3)) => 5
    case () => h
  }
}
