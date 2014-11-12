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
}
