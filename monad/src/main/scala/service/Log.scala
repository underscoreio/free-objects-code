package service

import scalaz.{Free, ~>, Id, Functor}

sealed trait Log[A]
final case class Debug[A](msg: String, value: A) extends Log[A]
final case class Warn[A](msg: String, value: A) extends Log[A]
final case class Error[A](msg: String, value: A) extends Log[A]

object Log {
  implicit val logFunctor: Functor[Log] = new Functor[Log] {
    def map[A, B](fa: Log[A])(f: A => B): Log[B] =
      fa match {
        case Debug(msg, value) => Debug(msg, f(value))
        case Warn(msg, value) => Warn(msg, f(value))
        case Error(msg, value) => Error(msg, f(value))
      }
  }

  // Smart constructors
  def debug[A](msg: String, value: A): Log[A] = Debug(msg, value)
  def warn[A](msg: String, value: A): Log[A] = Warn(msg, value)
  def error[A](msg: String, value: A): Log[A] = Error(msg, value)
}

object Println extends (Log ~> Id.Id) {
  import Id._
  import scalaz.syntax.monad._

  def apply[A](in: Log[A]): Id[A] =
    in match {
      case Debug(msg, value) =>
        println(s"DEBUG: $msg")
        value.point[Id]

      case Warn(msg, value) =>
        println(s"WARN: $msg")
        value.point[Id]

      case Error(msg, value) =>
        println(s"ERROR: $msg")
        value.point[Id]
    }
}

object LogExample {
  val free =
    for {
      x <- Free.liftF(Log.debug("Step 1", 1))
      y <- Free.liftF(Log.warn("Step 2", 2))
      z <- Free.liftF(Log.error("Step 3", 3))
    } yield x + y + z

  val result =
    free.foldMap(Println)
}
