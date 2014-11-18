package service

import scala.language.higherKinds
import scalaz.{Free, ~>, Id, Functor, Inject}

final case class Credentials(email: String, password: String)

sealed trait Auth[A]
final case class GetCredentials[A](next: Credentials => A) extends Auth[A]
final case class Login[A](credentials: Credentials, next: Option[User] => A) extends Auth[A]
final case class Logout[A](credentials: Credentials, next: Boolean => A) extends Auth[A]

object Auth {
  implicit val authFunctor: Functor[Auth] = new Functor[Auth] {
    def map[A, B](fa: Auth[A])(f: A => B): Auth[B] =
      fa match {
        case GetCredentials(next) => GetCredentials(next andThen f)
        case Login(credentials, next) => Login(credentials, next andThen f)
        case Logout(credentials, next) => Logout(credentials, next andThen f)
      }
  }

  def getCredentials[F[_]: Functor](implicit I: Inject[Auth, F]): Free[F, Credentials] =
    Inject.inject[F, Auth, Credentials](GetCredentials(Free.point(_)))
  def login[F[_]: Functor](credentials: Credentials)(implicit I: Inject[Auth, F]): Free[F, Option[User]] =
    Inject.inject[F, Auth, Option[User]](Login(credentials, Free.point(_)))
  def logout[F[_]: Functor](credentials: Credentials)(implicit I: Inject[Auth, F]): Free[F, Boolean] =
    Inject.inject[F, Auth, Boolean](Logout(credentials, Free.point(_)))
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
      case Login(credentials, next) =>
        if((credentials.email == dummyUser.email) &&
           (credentials.password == dummyUser.password))
          next(some(dummyUser))
        else
          next(none)
      case Logout(credentials, next) => next(true)
    }
}

object AuthExample {
  import Auth._

  val free =
    for {
      c <- getCredentials
      u <- login(c)
      l <- logout(c)
    } yield l
}
