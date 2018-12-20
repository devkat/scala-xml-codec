package ch.srf.xml

import scalaz.{Applicative, Functor, Monad}
import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import scalaz.syntax.functor._
import scalaz.syntax.monad._

sealed trait CardinalityPredicate[F[_], C[_]] {
  def apply[A](name: String, as: C[A], p: A => Result[F, Boolean]): A => Result[F, C[A]]
}

object CardinalityPredicate {
/*
  private def flatMapResult[F[_]:Monad, A, B](result: Result[F, A])(f: A => Result[F, B]): Result[F, B] =
    result.monadic.flatMap(f(_).monadic).applicative

  implicit def single[F[_]:Monad, X]: CardinalityPredicate[F, X, X] =
    new CardinalityPredicate[F, X] {
      override def apply(name: String, x: X, p: X => Result[F, Boolean]): Result[F, X] =
        flatMapResult(p(x))(b => Result.fromDisjunction(b.either(x).or(s"Predicate delivered no match").point[F], name))
    }

  implicit def option[F[_], X]: CardinalityPredicate[F, Option[X]] =
    new CardinalityPredicate[F, Option[X]] {
      override def apply(name: String, x: Option[X], p: Option[X] => Result[F, Boolean]): Result[F, Option[X]] =
        p(x).map(_.fold(x, None))
    }

  implicit def list[F[_], X]: CardinalityPredicate[F, List[X]] =
    new CardinalityPredicate[F, List[X]] {
      override def apply(name: String, x: List[X], p: List[X] => Result[F, Boolean]): Result[F, List[X]] =
        flatMapResult(p(x))(b => Result.fromDisjunction(b.either(x).or(s"Predicate delivered no match").point[F], name))
    }
*/
}