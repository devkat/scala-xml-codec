package ch.srf.xml

import ch.srf.xml.util.{CompactHList, Flatten}
import scalaz.std.string.stringInstance
import scalaz.syntax.all._
import scalaz.syntax.tag._
import scalaz.{@@, Monad, NonEmptyList, \/}

import scala.xml.Elem

final case class XmlDecoder[F[_]:Monad, D, X, A](descriptor: Descriptor[D],
                                                 dec: X => Result[F, A],
                                                 predicate: X => Result[F, Boolean]) {

  def as[B](implicit dec: Decoder[F, A, B]): XmlDecoder[F, D, X, B] =
    this ~ dec

  def ~[B](d: Decoder[F, A, B]): XmlDecoder[F, D, X, B] =
    XmlDecoder[F, D, X, B](
      descriptor,
      dec(_).monadic.flatMap(a => Result.fromDisjunction(d.decode(a), descriptor.name).monadic).applicative,
      predicate
    )

  def ensure(e: Ensure[F, A]): XmlDecoder[F, D, X, A] =
    this ~ Decoder.ensure(e)

  def skip[B](implicit ev: Flatten[A, B]): XmlDecoder[F, D, X, B] =
    this ~ Decoder.fromFunction(ev.to)

  def decode(x: X): F[NonEmptyList[String] \/ A] =
    dec(x).leftAsStrings

  def decodeFromParent(e: Elem)(implicit ev: GetFromElem[D, X]): F[NonEmptyList[String] \/ A] = {
    val decoder = Decoder.fromDisjunction[F, Elem, X](e => ev(e, descriptor.identifier))
    Result.fromDisjunction(decoder.decode(e), descriptor.name).monadic.flatMap(dec(_).monadic).applicative.leftAsStrings
  }

  def when(predicate: XmlDecoder[F, D, X, Boolean]): XmlDecoder[F, D, X, A] =
    copy(predicate = predicate.dec)

}

object XmlDecoder {

  def collection[F[_]:Monad, C[_], D, X, A](d: XmlDecoder[F, D, X, A],
                                            cd: CardinalityDecoder[F, C, X, A]): XmlDecoder[F, D, C[X], C[A]] =
    XmlDecoder[F, D, C[X], C[A]](
      d.descriptor,
      x => cd.wrapPredicate(d.predicate)(x).monadic.flatMap(y => cd.decode(d.dec, y).monadic).applicative,
      _ => Result.success(true)
    )

  private def textDecoder[F[_]:Monad, T]: XmlDecoder[F, Unit, String @@ T, String] =
    XmlDecoder[F, Unit, String @@ T, String](
      Descriptor.text,
      x => Result.success(x.unwrap).prependPath(Descriptor.text.name, None),
      _ => Result.success(true)
    )

  def text[F[_]:Monad]: XmlDecoder[F, Unit, String @@ TextValue, String] =
    textDecoder[F, TextValue]

  def nonEmptyText[F[_]:Monad]: XmlDecoder[F, Unit, String @@ NonEmptyTextValue, String] =
    textDecoder[F, NonEmptyTextValue]

  def attr[F[_]:Monad](name: String): XmlDecoder[F, String, String @@ AttrValue, String] =
    XmlDecoder[F, String, String @@ AttrValue, String](
      Descriptor.attr(name),
      a => Result.success(a.unwrap).prependPath(name, None),
      _ => Result.success(true)
    )

  def elem[F[_]:Monad, CS, C, A](name: String, children: CS)
                                (implicit
                                 hListDecoder: HListDecoder[F, CS, C],
                                 compact: CompactHList[C, A]): XmlDecoder[F, String, Elem, A] = {

    def checkName: Decoder[F, Elem, Elem] =
      Decoder.ensure[F, Elem](EnsureOps.check(_.label === name, e => s"Found <${e.label}> instead of <$name>"))

    XmlDecoder[F, String, Elem, A](
      Descriptor.elem(name),
      e => Result
        .fromDisjunction(checkName.decode(e), name)
        .monadic
        .flatMap(_ => hListDecoder(children, e).prependPath(name, None).monadic)
        .map(compact.to)
        .applicative,
      _ => Result.success(true)
    )
  }

}