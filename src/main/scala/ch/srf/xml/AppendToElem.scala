package ch.srf.xml

import scalaz.std.list.listInstance
import scalaz.std.option.optionInstance
import scalaz.syntax.foldable._
import scalaz.syntax.tag._
import scalaz.{@@, Foldable, NonEmptyList}

import scala.xml.{Attribute, Elem, Null, Text}

private[xml] sealed trait AppendToElem[X] {

  def apply(elem: Elem, x: X, name: String): Elem

}

private[xml] object AppendToElem {

  private def apply[X](f: (Elem, X, String) => Elem): AppendToElem[X] =
    new AppendToElem[X] {
      override def apply(elem: Elem, x: X, name: String): Elem =
        f(elem, x, name)
    }

  /* ----- Attributes ----- */

  implicit def attrInstance: AppendToElem[String @@ AttrValue] =
    apply((e, a, name) => e % Attribute(None, name, Text(a.unwrap), Null))

  implicit def attrOptionInstance[S]: AppendToElem[Option[String @@ AttrValue]] =
    apply ((e, a, name) => a.map(s => e % Attribute(None, name, Text(s.unwrap), Null)).getOrElse(e))

  /* ----- Text ----- */

  private def appendText[T](e: Elem)(a: String @@ T) =
    e.copy(child = e.child :+ Text(a.unwrap))

  implicit def textValueInstance: AppendToElem[String @@ TextValue] =
    apply((e, a, _) => appendText(e)(a))

  implicit def nonEmptyTextValueInstance: AppendToElem[String @@ NonEmptyTextValue] =
    apply((e, a, _) => appendText(e)(a))

  implicit def optionalNonEmptyTextValueInstance: AppendToElem[Option[String @@ NonEmptyTextValue]] =
    apply((e, a, _) => a.map(appendText(e)).getOrElse(e))

  /* ----- Elements ----- */

  implicit def elemInstance: AppendToElem[Elem] =
    apply((e, a, _) => e.copy(child = a +: e.child))

  def elemsInstance[Cy[_]:Foldable]: AppendToElem[Cy[(Elem, Option[Int])]] =
    apply((parent, a, _) => a.foldRight(parent)((e, p) => p.copy(child = e._1 +: p.child)))

  implicit def elemOptionInstance: AppendToElem[Option[Elem]] =
    apply((e, a, _) => a.fold(e)(elem => e.copy(child = elem +: e.child)))

  implicit def elemListInstance: AppendToElem[List[(Elem, Option[Int])]] =
    elemsInstance[List]

  implicit def elemNelInstance: AppendToElem[NonEmptyList[(Elem, Option[Int])]] =
    elemsInstance[NonEmptyList]

}