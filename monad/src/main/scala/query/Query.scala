package query

import scalaz.Functor

sealed trait Query[A]
final case class ForEach[S,A](source: List[S], next: Ref[S] => A) extends Query[A]
final case class Filter[A](pred: Expr[Bool], next: Bool => A) extends Query[A]
final case class Select[S,A](expr: Expr[S], next: S => A) extends Query[A]

sealed trait Expr[A]
final case class Ref[A](name: String) extends Expr[A]
final case class Gt(left: Expr[Num], right: Expr[Num]) extends Expr[Boolean]
final case class Lt(left: Expr[Num], right: Expr[Num]) extends Expr[Boolean]
final case class Add(left: Expr[Num], right: Expr[Num]) extends Expr[Num]
final case class Num(value: Int) extends Expr[Num]
final case class Bool(value: Boolean) extends Expr[Bool]

// select a from someTable where a < 2
// ForEach(a <- someTable)
// Filter(a < 2)
// Select(a)

object Query {
  implicit val queryFunctor: Functor[Query] = new Functor[Query] {
    def map[A, B](fa: Query[A])(f: A => B): Query[B] =
      fa match {
        case ForEach(source, next) => ForEach(source, next andThen f)//(ref: Ref) => f(next(ref)))
        case Filter(pred, next) => Filter(pred, bool => f(next(bool)))
        case Select(expr, next) => Select(expr, next andThen f)//a => f(next(a)))
      }
  }
}
