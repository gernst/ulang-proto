import java.io.Reader

import arse._

package object ulang {
  import arse.Recognizer._
  
  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")
  
  def identity[A](a: A) = a
  
  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }
}