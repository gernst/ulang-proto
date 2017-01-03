import java.io.Reader

import arse._
import scala.collection.mutable.ListBuffer
import java.io.StringReader
import java.io.FileReader
import java.io.File

package object ulang {
  import arse.Recognizer._

  def expect(s: String) = s ! "expected '" + s + "'"
  def expect[A](s: String, p: Parser[List[String], A]) = p ! "expected "
  def parens[A](s0: String, p: Parser[List[String], A], s1: String) = s0 ~ p ~ expect(s1)


  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

  case class Ref[A](var get: A) {
    def set(a: A) { get = a }
    def map(f: A => A) { get = f(get) }
  }

  type Env = Map[String, Val]
  type Subst = Map[Expr, Expr]
}