package service

import scala.language.higherKinds
import scalaz.{Coproduct, ~>}

object Combination {
  import Auth._
  import Log._

  type T[A] = Coproduct[Auth, Log, A]

  implicit class NaturalTransformationOr[F[_], G[_]](self: F ~> G) {
    import scalaz.{-\/, \/-}

    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case -\/(fa) => self(fa)
          case \/-(ha) => f(ha)
        }
      }
  }


  val free =
    for {
      _    <- debug[T]("Getting credentials")
      c    <- getCredentials[T]
      _    <- debug[T]("Attempting login")
      optU <- login[T](c)
      _    <- if(optU.isDefined) debug[T]("Logged in") else debug[T]("Could not login")
    } yield c

  def result() = free foldMap(DummyAuth or Println)
}
