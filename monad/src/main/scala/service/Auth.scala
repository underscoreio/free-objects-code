package service

import scala.language.higherKinds
import scalaz.{Free, ~>, Id, Functor, Inject}

final case class Credentials(email: String, password: String)
sealed trait Action
final case object Read extends Action
final case object Write extends Action

sealed trait Auth[A]
final case class GetCredentials[A](next: Credentials => A) extends Auth[A]
final case class Authenticate[A](credentials: Credentials, next: Option[User] => A) extends Auth[A]
final case class Authorize[A](credentials: User, action: Action, next: Boolean => A) extends Auth[A]

object Auth {
  implicit val authFunctor: Functor[Auth] = new Functor[Auth] {
    def map[A, B](fa: Auth[A])(f: A => B): Auth[B] =
      fa match {
        case GetCredentials(next) => GetCredentials(next andThen f)
        case Authenticate(credentials, next) => Authenticate(credentials, next andThen f)
        case Authorize(credentials, action, next) => Authorize(credentials, action, next andThen f)
      }
  }

  def getCredentials[F[_]: Functor](implicit I: Inject[Auth, F]): Free[F, Credentials] =
    Inject.inject[F, Auth, Credentials](GetCredentials(Free.point(_)))
  def authenticate[F[_]: Functor](credentials: Credentials)(implicit I: Inject[Auth, F]): Free[F, Option[User]] =
    Inject.inject[F, Auth, Option[User]](Authenticate(credentials, Free.point(_)))
  def authorize[F[_]: Functor](user: User, action: Action)(implicit I: Inject[Auth, F]): Free[F, Boolean] =
    Inject.inject[F, Auth, Boolean](Authorize(user, action, Free.point(_)))
}


object DummyAuth extends (Auth ~> Id.Id) {
  import Id._
  import scalaz.syntax.monad._
  import scalaz.std.option._

  val dummyUser = User("Dumbo", "dummy@example.com", "password")
  val dummyCredentials = Credentials(dummyUser.email, dummyUser.password)

  def apply[A](in: Auth[A]): Id[A] =
    in match {
      case GetCredentials(next) => next(dummyCredentials)
      case Authenticate(credentials, next) =>
        if((credentials.email == dummyUser.email) &&
           (credentials.password == dummyUser.password))
          next(some(dummyUser))
        else
          next(none)
      case Authorize(user, action, next) => next(true)
    }
}

object AuthExample {
  import Auth._
  import scalaz.syntax.monad._
  type Action[A] = Free[Auth, A]

  val free =
    for {
      c <- getCredentials
      u <- authenticate(c)
      p <- u.fold(false.point[Action])(u => authorize(u, Read))
    } yield p
}
