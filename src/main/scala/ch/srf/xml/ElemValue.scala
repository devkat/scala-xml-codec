package ch.srf.xml

import scalaz.Show

import scala.xml.Elem

private[xml] final case class ElemValue(name: String, pos: Int, value: Elem)

object ElemValue {

  implicit lazy val show: Show[ElemValue] =
    Show.shows(e => e.name + "[" + e.pos.toString + "]")

}
