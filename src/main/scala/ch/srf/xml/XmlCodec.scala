package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.{@@, Monad, NonEmptyList, Traverse, \/}

import scala.xml.Elem

final class XmlCodec[F[_]:Monad, X, A](val decoder: XmlDecoder[F, X, A],
                                          val encoder: XmlEncoder[F, X, A]) {

  def as[B](implicit codec: Codec[F, A, B]): XmlCodec[F, X, B] =
    this ~ codec

  def ~[B](codec: Codec[F, A, B]): XmlCodec[F, X, B] =
    new XmlCodec[F, X, B](
      decoder ~ codec.decoder,
      encoder ~ codec.encoder)

  def ensure(e: Ensure[F, A]): XmlCodec[F, X, A] =
    new XmlCodec(decoder.ensure(e), encoder)

  def skip[B](implicit ev: Flatten[A, B]): XmlCodec[F, X, B] =
    new XmlCodec(decoder.skip, encoder.skip)

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    decoder.decode(x)

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[X]): F[NonEmptyList[String] \/ A] =
    decoder.decodeFromParent(e)

  def encode(a: A): F[X] =
    encoder.encode(a)

}

object XmlCodec {

  def option[F[_]:Monad, X, A](codec: XmlCodec[F, X, A]): XmlCodec[F, Option[X], Option[A]] =
    new XmlCodec[F, Option[X], Option[A]](
      XmlDecoder.option[F, X, A](codec.decoder),
      XmlEncoder.option[F, X, A](codec.encoder)
    )

  def list[F[_]:Monad, X, A](codec: XmlCodec[F, X, A]): XmlCodec[F, List[(X, Option[Int])], List[A]] =
    new XmlCodec[F, List[(X, Option[Int])], List[A]](
      XmlDecoder.list[F, X, A](codec.decoder),
      XmlEncoder.list[F, X, A](codec.encoder)
    )

  def nel[F[_]:Monad, X, A](codec: XmlCodec[F, X, A]): XmlCodec[F, NonEmptyList[(X, Option[Int])], NonEmptyList[A]] =
    new XmlCodec[F, NonEmptyList[(X, Option[Int])], NonEmptyList[A]](
      XmlDecoder.nel[F, X, A](codec.decoder),
      XmlEncoder.nel[F, X, A](codec.encoder)
    )

  def text[F[_]:Monad]: XmlCodec[F, String @@ TextValue, String] =
    new XmlCodec(XmlDecoder.text, XmlEncoder.text)

  def nonEmptyText[F[_]:Monad]: XmlCodec[F, String @@ NonEmptyTextValue, String] =
    new XmlCodec(XmlDecoder.nonEmptyText, XmlEncoder.nonEmptyText)

  def attr[F[_]:Monad](name: String): XmlCodec[F, String @@ AttrValue, String] =
    new XmlCodec(XmlDecoder.attr(name), XmlEncoder.attr(name))

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 hListEncoder: HListEncoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlCodec[F, Elem, A] =
    new XmlCodec(XmlDecoder.elem(name, children), XmlEncoder.elem(name, children))

}