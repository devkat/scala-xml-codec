package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.contravariant._
import scalaz.syntax.traverse._
import scalaz.{@@, Monad, NonEmptyList, Tag, Traverse}

import scala.xml.Elem

final case class XmlEncoder[F[_]:Monad, X, A](name: String,
                                              encoder: Encoder[F, X, A]) {
  def as[B](implicit enc: Encoder[F, A, B]): XmlEncoder[F, X, B] =
    this ~ enc

  def ~[B](enc: Encoder[F, A, B]): XmlEncoder[F, X, B] =
    XmlEncoder[F, X, B](
      name,
      encoder ~ enc
    )

  def skip[B](implicit ev: Flatten[A, B]): XmlEncoder[F, X, B] =
    this ~ Encoder.fromFunction(ev.from)


  def encode(a: A): F[X] =
    encoder.encode(a)
}

object XmlEncoder {

  def option[F[_]:Monad, X, A](enc: XmlEncoder[F, X, A]): XmlEncoder[F, Option[X], Option[A]] =
    XmlEncoder[F, Option[X], Option[A]](
      enc.name,
      Encoder(_.traverse(enc.encoder.encode))
    )

  def list[F[_]:Monad, X, A](enc: XmlEncoder[F, X, A]): XmlEncoder[F, List[(X, Option[Int])], List[A]] =
    XmlEncoder[F, List[(X, Option[Int])], List[A]](
      enc.name,
      traverseEncoder(enc)
    )

  def nel[F[_]:Monad, X, A](enc: XmlEncoder[F, X, A]): XmlEncoder[F, NonEmptyList[(X, Option[Int])], NonEmptyList[A]] =
    XmlEncoder[F, NonEmptyList[(X, Option[Int])], NonEmptyList[A]](
      enc.name,
      traverseEncoder(enc)
    )

  private def traverseEncoder[
  F[_]:Monad,
  T[_]:Traverse, X, A
  ](enc: XmlEncoder[F, X, A]): Encoder[F, T[(X, Option[Int])], T[A]] =
    Encoder(_.traverse(enc.encoder.encode).map(_.map((_, Option.empty[Int]))))

  private def textEncoder[F[_]:Monad, T]: XmlEncoder[F, String @@ T, String] =
    XmlEncoder(
      "",
      Encoder.fromFunction(v => Tag.of[T](v))
    )

  def text[F[_]:Monad]: XmlEncoder[F, String @@ TextValue, String] =
    textEncoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlEncoder[F, String @@ NonEmptyTextValue, String] =
    textEncoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlEncoder[F, String @@ AttrValue, String] =
    XmlEncoder(
      name,
      Encoder.fromFunction(v => Tag.of[AttrValue](v))
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlEncoder[F, Elem, A] = {
    def compactEncoder: Encoder[F, C, A] =
      Encoder.fromFunction[F, C, A](compact.from)

    XmlEncoder(
      name,
      hListEncoder.apply(children).contramap[C]((<dummy/>.copy(label = name), _)) ~ compactEncoder
    )

  }

}