import java.io.Reader

import arse._
import scala.collection.mutable.ListBuffer
import java.io.StringReader
import java.io.FileReader
import java.io.File

package object ulang {
  import arse.Recognizer._

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")

  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

  val Wildcard = Id("_")

  val True = Tag("True")
  val False = Tag("False")

  case class Ref[A](var get: A) {
    def set(a: A) { get = a }
    def map(f: A => A) { get = f(get) }
  }

  type Env = Map[String, Val]
  type Subst = Map[Expr, Expr]
}