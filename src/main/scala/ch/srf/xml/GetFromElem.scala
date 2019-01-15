package ch.srf.xml

import scalaz.syntax.either._
import scalaz.syntax.std.list._
import scalaz.syntax.std.option._
import scalaz.{@@, NonEmptyList, Tag, \/}

import scala.xml.Elem

private[xml] sealed trait GetFromElem[X] {

  def apply(e: Elem, name: String): String \/ X

}

private[xml] object GetFromElem {

  private def apply[X](f: (Elem, String) => String \/ X): GetFromElem[X] =
    new GetFromElem[X] {
      override def apply(e: Elem, name: String): String \/ X =
        f(e, name)
    }

  /* ----- Attributes ----- */

  def getAttribute(e: Elem, name: String): Option[String @@ AttrValue] =
    e.attributes.asAttrMap.get(name).map(Tag.of[AttrValue](_))

  implicit lazy val attrInstance: GetFromElem[String @@ AttrValue] =
    apply((elem, name) => getAttribute(elem, name).\/>(s"Attribute '$name' missing"))

  implicit lazy val attrOptionInstance: GetFromElem[Option[String @@ AttrValue]] =
    apply((elem, name) => getAttribute(elem, name).right)

  /* ----- Text ----- */

  def nonEmptyTextValue(e: Elem): Option[String @@ NonEmptyTextValue] = {
    Option(e.text).filter(!_.isEmpty).map(Tag.of[NonEmptyTextValue](_))
  }

  implicit lazy val nonEmptyTextInstance: GetFromElem[String @@ NonEmptyTextValue] =
    apply((elem, _) => nonEmptyTextValue(elem).\/>("Text must not be empty"))

  implicit lazy val textInstance: GetFromElem[String @@ TextValue] =
    apply((elem, _) => Tag.of[TextValue](elem.text).right)

  implicit lazy val nonEmptyTextOptionInstance: GetFromElem[Option[String @@ NonEmptyTextValue]] =
    apply((elem, _) => nonEmptyTextValue(elem).right)

  /* ----- Elements ----- */

  def elems(parent: Elem, name: String): List[(Elem, Option[Int])] = {
    val elems = (parent \ name).toList.collect { case e: Elem => e }
    if (elems.size > 1)
      elems.zipWithIndex.map { case (e, p) => (e, Some(p + 1)) }
    else
      elems.map((_, Option.empty[Int]))
  }

  implicit lazy val elemInstance: GetFromElem[Elem] =
    apply((elem, name) => elems(elem, name) match {
      case h :: Nil => h._1.right
      case l => s"Exactly one element <$name> expected, found ${l.size}".left
    })

  implicit def elemOptionInstance[CS]: GetFromElem[Option[Elem]] =
    apply((elem, name) => elems(elem, name) match {
      case Nil => None.right
      case List(h) => Some(h._1).right
      case l => s"At most one element <$name> expected, found ${l.size}".left
    })

  implicit def elemListInstance[CS]: GetFromElem[List[(Elem, Option[Int])]] =
    apply((elem, name) => elems(elem, name).right)

  implicit def elemNelInstance[CS]: GetFromElem[NonEmptyList[(Elem, Option[Int])]] =
    apply((elem, name) => elems(elem, name).toNel.\/>(s"At least one element <$name> expected"))

}
