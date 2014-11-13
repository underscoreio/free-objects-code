package option

import scalaz.{~>, Free}
import scalaz.std.option._

object Examples {

  def normal = {
    for {
      x <- Some(1)
      y <- Some(2)
      z <- Some(3)
    } yield x + y + z
  }

  def free = {
    for {
      x <- Free.liftF(some(1))
      y <- Free.liftF(some(2))
      z <- Free.liftF(some(3))
    } yield x + y + z
  }

  val idExe: Option ~> Option = new (Option ~> Option) {
    def apply[A](in: Option[A]): Option[A] =
      in
  }

  val listExe: Option ~> List = new (Option ~> List) {
    def apply[A](in: Option[A]): List[A] =
      in map (List(_)) getOrElse Nil
  }

  import scalaz.DList
  final case class Timer[A](times: DList[Long], value: Option[A])
  object Timer {
    import scalaz.Monad

    implicit val logMonad = new Monad[Timer] {
      def bind[A, B](fa: Timer[A])(f: (A) ⇒ Timer[B]): Timer[B] = {
        val now = System.nanoTime()
        val result = fa.value.map(f).getOrElse(Timer(DList(), None))
        Timer((fa.times :+ now) ++ result.times, result.value)
      }
      def point[A](a: ⇒ A): Timer[A] = {
        Timer(DList(System.nanoTime()), some(a))
      }
    }
  }

  val timerExe: Option ~> Timer = new (Option ~> Timer) {
    def apply[A](in: Option[A]): Timer[A] =
      Timer(DList(), in)
  }
}
