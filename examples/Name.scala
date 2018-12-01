object lol {
  abstract class H
  case class h1 () extends H
  case class h2 (h: H) extends H
  case class h3 (b: Boolean) extends H


  val p: H = h2(h1());
  ()
}
