package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Applicative, Monad, NonEmptyList, Traverse, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_]:Monad, X, A](name: String,
                                              segment: String,
                                              dec: X => Result[F, A]) {

  def as[B](implicit dec: Decoder[F, A, B]): XmlDecoder[F, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, A, B]): XmlDecoder[F, X, B] =
    XmlDecoder(
      name,
      segment,
      dec(_).monadic.flatMap(a => Result.fromDisjunction(d.decode(a), segment).monadic).applicative
    )

  def ensure(e: Ensure[F, A]): XmlDecoder[F, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit ev: Flatten[A, B]): XmlDecoder[F, X, B] =
    this ~ Decoder.fromFunction(ev.to)

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    dec(x).leftAsStrings

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[X]): F[NonEmptyList[String] \/ A] = {
    val decoder = Decoder.fromDisjunction[F, Elem, X](e => ev(e, name))
    Result.fromDisjunction(decoder.decode(e), segment).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings
  }

}

object XmlDecoder {

  def option[F[_]:Monad, X, A](d: XmlDecoder[F, X, A]): XmlDecoder[F, Option[X], Option[A]] = {
    import scalaz.std.option.optionInstance
    XmlDecoder[F, Option[X], Option[A]](
      d.name,
      d.segment,
      _ traverse d.dec
    )
  }

  def list[F[_]:Monad, X, A](d: XmlDecoder[F, X, A]): XmlDecoder[F, List[(X, Option[Int])], List[A]] = {
    import scalaz.std.list.listInstance
    XmlDecoder[F, List[(X, Option[Int])], List[A]](
      d.name,
      d.segment,
      decodeTraverse(d.dec, _)
    )
  }

  def nel[F[_]:Monad, X, A](d: XmlDecoder[F, X, A]): XmlDecoder[F, NonEmptyList[(X, Option[Int])], NonEmptyList[A]] =
    XmlDecoder[F, NonEmptyList[(X, Option[Int])], NonEmptyList[A]](
      d.name,
      d.segment,
      decodeTraverse(d.dec, _)
    )

  private def decodeTraverse[
  F[_]: Applicative,
  G[_]: Traverse, X, A](dec: X => Result[F, A],
                        xs: G[(X, Option[Int])]): Result[F, G[A]] =
    xs.traverse { case (e, pos) => pos.fold(dec(e))(dec(e).updatePos) }


  private def textDecoder[F[_]:Monad, T]: XmlDecoder[F, String @@ T, String] =
    XmlDecoder(
      "",
      "<text>",
      x => Result.success(x.unwrap).prependPath("", None)
    )

  def text[F[_]:Monad]: XmlDecoder[F, String @@ TextValue, String] =
    textDecoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, String @@ NonEmptyTextValue, String] =
    textDecoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlDecoder[F, String @@ AttrValue, String] =
    XmlDecoder(
      name,
      "@" + name,
      x => Result.success(x.unwrap).prependPath("@" + name, None)
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlDecoder[F, Elem, A] = {

    def checkName: Decoder[F, Elem, Elem] =
      Decoder.ensure[F, Elem](EnsureOps.check(_.label === name, e => s"Found <${e.label}> instead of <$name>"))

    XmlDecoder(
      name,
      name,
      e => Result
        .fromDisjunction(checkName.decode(e), name)
        .monadic
        .flatMap(_ => hListDecoder(children, e).prependPath(name, None).monadic)
        .map(compact.to)
        .applicative
    )

  }

}