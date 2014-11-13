package id

import scalaz.{Free, Id, NaturalTransformation}
import scalaz.syntax.monad._

object IdExample {
  import Id._

  val free =
    for {
      x <- Free.liftF(1.point[Id])
      y <- Free.liftF(2.point[Id])
      z <- Free.liftF(3.point[Id])
    } yield x + y + z

  val result =
    free.foldMap(NaturalTransformation.id)
}
