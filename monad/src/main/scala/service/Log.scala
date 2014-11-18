package service

import scala.language.higherKinds
import scalaz.{Free, ~>, Id, Functor, Inject}

sealed trait Log[A]
final case class Debug[A](msg: String, next: A) extends Log[A]
final case class Warn[A](msg: String, next: A) extends Log[A]
final case class Error[A](msg: String, next: A) extends Log[A]

object Log {
  implicit val logFunctor: Functor[Log] = new Functor[Log] {
    def map[A, B](fa: Log[A])(f: A => B): Log[B] =
      fa match {
        case Debug(msg, next) => Debug(msg, f(next))
        case Warn(msg, next) => Warn(msg, f(next))
        case Error(msg, next) => Error(msg, f(next))
      }
  }

  // Smart constructors
  def debug[F[_]: Functor](msg: String)(implicit I: Inject[Log, F]): Free[F, Unit] =
    Inject.inject[F, Log, Unit](Debug(msg, Free.point(())))
  def warn[F[_]: Functor](msg: String)(implicit I: Inject[Log, F]): Free[F, Unit] =
    Inject.inject[F, Log, Unit](Warn(msg, Free.point(())))
  def error[F[_]: Functor](msg: String)(implicit I: Inject[Log, F]): Free[F, Unit] =
    Inject.inject[F, Log, Unit](Error(msg, Free.point(())))
}

object Println extends (Log ~> Id.Id) {
  import Id._
  import scalaz.syntax.monad._

  def apply[A](in: Log[A]): Id[A] =
    in match {
      case Debug(msg, next) =>
        println(s"DEBUG: $msg")
        next.point[Id]

      case Warn(msg, next) =>
        println(s"WARN: $msg")
        next.point[Id]

      case Error(msg, next) =>
        println(s"ERROR: $msg")
        next.point[Id]
    }
}

object LogExample {
  import Log._

  val free =
    for {
      _ <- debug("Step 1")
      x = 1
      _ <- warn("Step 2")
      y = 2
      _ <- error("Step 3")
      z = 3
    } yield x + y + z

  val result =
    free.foldMap(Println)
}
