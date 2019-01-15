package ch.srf.xml

import scalaz.std.anyVal.intInstance
import scalaz.std.option.optionEqual
import scalaz.std.string.stringInstance
import scalaz.std.tuple._
import scalaz.syntax.foldable1._
import scalaz.{Equal, NonEmptyList, Show}

private[xml] final case class Path(elems: NonEmptyList[(String, Option[Int])]) {

  def prepend(segment: String, position: Option[Int]): Path =
    Path((s"$segment", position) <:: elems)

  def updatePos(pos: Int): Path = {
    val (segment, _) = elems.head
    Path(NonEmptyList.nel((segment, Some(pos)), elems.tail))
  }

}

private[xml] object Path {

  implicit def showInstance: Show[Path] =
    new Show[Path] {
      override def shows(path: Path): String =
        path.elems.map { case (segment, pos) => segment + pos.fold("")("[" + _.toString + "]") }.foldLeft1(_ + "/" + _)
    }

  implicit def equalInstance: Equal[Path] =
    Equal.equalBy[Path, NonEmptyList[(String, Option[Int])]](_.elems)

}
