import scalaz.Monoid

object FoldMonoid {
  def apply[A](in: List[A])(implicit monoid: Monoid[A]) =
    in.foldLeft(monoid.zero)((x, y) => monoid.append(x, y))
}

object Examples {
  import scalaz.std.anyVal._
  import scalaz.std.string._

  def examples = {
    FoldMonoid[Int](List(1, 2, 3))(Monoid[Int])
    FoldMonoid[Int](List(1, 2, 3))
    FoldMonoid[String](List("a", "b", "c"))
  }

}
