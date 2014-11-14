package service

import scalaz.{Free, ~>, Id, Functor}

final case class Credentials(email: String, password: String)

sealed trait Auth[A]
final case class GetCredentials[A](f: Credentials => A) extends Auth[Credentials => A]
final case class Login(credentials: Credentials, value: A) extends Auth[A]
final case class Logout(credentials: Credentials, value: A) extends Auth[A]

object Auth {
  implicit val authFunctor: Functor[Auth] = new Functor[Auth] {
    def map[A, B](fa: Auth[A])(f: A => B): Auth[B] =
      fa match {
        case GetCredentials(value) => GetCredentials(creds => f(value(creds)))
        case Login(credentials, value) => Login(credentials, f(value))
        case Logout(credentials, value) => Logout(credentials, f(value))
      }
  }

  def getCredentials[A](value: A): Auth[A] = GetCredentials(value)
  def login[A](credentials: Credentials, value: A): Auth[A] = Login(credentials, value)
  def logout[A](credentials: Credentials, value: A): Auth[A] = Logout(credentials, value)
}


object DummyAuth extends (Credentials ~> Id.Id) {
  import Id
  import scalaz.syntax.monad._

  def apply[A](in: Auth[A]): Id[A] =
    in match {
      case GetCredentials(f) =>
        f(Credentials("dummy@example.com", "password"))
      case Login
    }
}
