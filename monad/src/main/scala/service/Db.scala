package service

import scala.language.higherKinds
import scalaz.{Free, ~>, Id, Functor, Inject}

sealed trait Db[K,V,A]
final case class Get[K,V,A](key: K, next: Option[V] => A) extends Db[K,V,A]
final case class Put[K,V,A](key: K, value: V, next: A) extends Db[K,V,A]

object Db {
  implicit def authFunctor[K, V]: Functor[({type l[A]=Db[K,V,A]})#l] = new Functor[({type l[A]=Db[K,V,A]})#l] {
    def map[A, B](fa: Db[K,V,A])(f: A => B): Db[K,V,B] =
      fa match {
        case Get(key, next) => Get(key, next andThen f)
        case Put(key, value, next) => Put(key, value, f(next))
      }
  }

  def get[K,V,F[_]: Functor](key: K)(implicit I: Inject[({type l[A]=Db[K,V,A]})#l, F]): Free[F, Option[V]] =
    Inject.inject[F, ({type l[A]=Db[K,V,A]})#l, Option[V]](Get(key, Free.point(_)))

  def put[K,V,F[_]: Functor](key: K, value: V)(implicit I: Inject[({type l[A]=Db[K,V,A]})#l, F]): Free[F, Unit] =
    Inject.inject[F, ({type l[A]=Db[K,V,A]})#l, Unit](Put(key, value, Free.point(())))
}


case class DummyDb[K,V]() extends (({type l[A]=Db[K,V,A]})#l ~> Id.Id) {
  import Id._
  import scalaz.syntax.monad._
  import scalaz.std.option._

  type TheDb[A] = Db[K,V,A]

  var store = Map.empty[K, V]

  def apply[A](in: TheDb[A]): Id[A] =
    in match {
      case Get(key, next) => next(store.get(key))
      case Put(key, value, next) =>
        store = (store + (key -> value))
        next
    }
}

object DbExample {
  import Db._
  import scalaz.syntax.monad._

  type TheDb[A] = Db[String, String, A]
  implicit val inject = Inject[TheDb, TheDb]

  val free =
    for {
      _ <- put[String, String, TheDb]("key", "value")
      a <- get[String, String, TheDb]("key")
      _ <- put[String, String, TheDb]("key", "invaluable")
      b <- get[String, String, TheDb]("key")
    } yield (a, b)
}
