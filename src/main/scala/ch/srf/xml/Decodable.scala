package ch.srf.xml

import scalaz.{Equal, Show}

import scala.xml.Elem

final case class Decodable[N, A](pathSegment: N => String)

object Decodable {

  val text: Decodable[Unit, String] =
    Decodable[Unit, String](pathSegment = _ => "<text>")

  val attr: Decodable[String, String] =
    Decodable[String, String](pathSegment = "@" + _)

  val elem: Decodable[(String, Int), Elem] =
    Decodable[(String, Int), Elem](pathSegment = { case (name, pos) => name + "[" + pos.toString + "]" })

  implicit def equalInstance[N, A]: Equal[Decodable[N, A]] =
    Equal.equalA

}
