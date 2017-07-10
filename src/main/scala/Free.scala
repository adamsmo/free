import scala.language.higherKinds

trait Monad[M[_]] {
  def pure[A](a: A): M[A]

  def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]
}

object Monad {
  def apply[A[_]]: Monad[A] = ???
}

trait ~>[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

trait Free[F[_], A] {

  import Free._

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Bind[F, A, B](this, f)

  def foldMap[G[_] : Monad](transformation: ~>[F, G]): G[A] = this match {
    case Lifted(fa) => transformation(fa)
    case Pure(a) => Monad[G].pure(a)
    //first transform self from F[Z] to G[Z] to be able to call flat map on G[Z] then inside flat map
    // call function f from Z to Free[F,B] and then transform Free[F,B] to G[B]
    case Bind(m, f) => Monad[G].flatMap(m.foldMap(transformation)) { e =>
      f(e).foldMap(transformation)
    }
  }
}

object Free {
  def pure[F[_], A](a: A): Free[F, A] = Pure(a)

  def lift[F[_], A](fa: F[A]): Free[F, A] = Lifted(fa)

  final case class Pure[F[_], A](a: A) extends Free[F, A]

  final case class Lifted[F[_], A](fa: F[A]) extends Free[F, A]

  //e is used to pass type from `Free.this` to f
  final case class Bind[F[_], e, A](`this`: Free[F, e], f: e => Free[F, A]) extends Free[F, A]

}