package service

import scalaz.{Free, ~>, Id, Functor}

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

  def getCredentials: Auth[Credentials] = GetCredentials(identity)
  def login(credentials: Credentials): Auth[Option[User]] = Login(credentials, identity)
  def logout(credentials: Credentials): Auth[Boolean] = Logout(credentials, identity)
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

object Example {
  import Auth._

  val free =
    for {
      c <- Free.liftF(getCredentials)
      u <- Free.liftF(login(c))
      l <- Free.liftF(logout(c))
    } yield l
}
