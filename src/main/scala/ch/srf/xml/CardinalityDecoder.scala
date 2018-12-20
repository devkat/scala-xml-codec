package ch.srf.xml

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.syntax.all._
import scalaz.{Applicative, Monad, NonEmptyList, Traverse}

private[xml] sealed trait CardinalityDecoder[F[_], C[_], X, A] {

  def decode(dec: X => Result[F, A], x: C[X]): Result[F, C[A]]

  def wrapPredicate(p: X => Result[F, Boolean]): C[X] => Result[F, C[X]]

}

private[xml] object CardinalityDecoder {

  def option[F[_]:Applicative, X, A]: CardinalityDecoder[F, Option, X, A] =
    new CardinalityDecoder[F, Option, X, A] {

      override def decode(dec: X => Result[F, A], x: Option[X]): Result[F, Option[A]] =
        x traverse dec

      override def wrapPredicate(p: X => Result[F, Boolean]): Option[X] => Result[F, Option[X]] =
        x => Applicative[Result[F, ?]].map(x.toList.filterM(p))(_.headOption)

    }

  def list[F[_]:Applicative, X, A]: CardinalityDecoder[F, List, X, A] =
    new CardinalityDecoder[F, List, X, A] {

      override def decode(dec: X => Result[F, A], xs: List[X]): Result[F, List[A]] =
        decodeTraverse(dec, xs)

      override def wrapPredicate(p: X => Result[F, Boolean]): List[X] => Result[F, List[X]] =
        _.filterM(p)

    }

  def nel[F[_]:Monad, X, A]: CardinalityDecoder[F, NonEmptyList, X, A] =
    new CardinalityDecoder[F, NonEmptyList, X, A] {

      override def decode(dec: X => Result[F, A], xs: NonEmptyList[X]): Result[F, NonEmptyList[A]] =
        decodeTraverse(dec, xs)

      override def wrapPredicate(p: X => Result[F, Boolean]): NonEmptyList[X] => Result[F, NonEmptyList[X]] =
        _.list.toList.filterM(p)
          .monadic
          .flatMap(l => Result.fromDisjunction(l.toNel.\/>("No match").point[F], "???").monadic)
          .applicative

    }

  private def decodeTraverse[
  F[_]: Applicative,
  G[_]: Traverse, X, A](dec: X => Result[F, A],
                        xs: G[X]): Result[F, G[A]] = {
    val pairs = xs.indexed.map { case (pos, x) => (x, pos + 1) }
    pairs.traverse { case (e, pos) => dec(e).updatePos(pos) }
  }

}
