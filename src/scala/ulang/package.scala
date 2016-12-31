import java.io.Reader

import arse._
import scala.collection.mutable.ListBuffer
import java.io.StringReader
import java.io.FileReader
import java.io.File
import ulang.core.Import

package object ulang {
  import arse.Recognizer._

  val languages: List[Language] = List(Import, core.Defs)

  def expect(s: String) = s ! "expected '" + s + "'"
  def parens[A](p: Parser[List[String], A]) = "(" ~ p ~ expect(")")

  def identity[A](a: A) = a

  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

  def tokenize(file: File): List[String] = {
    tokenize(new FileReader(file))
  }

  def tokenize(line: String): List[String] = {
    tokenize(new StringReader(line))
  }

  def tokenize(reader: Reader): List[String] = {
    val scanner = new Scanner(reader)
    val res = new ListBuffer[String]()
    var tok = scanner.next()
    while (tok != null) {
      res += tok
      tok = scanner.next()
    }
    res.toList
  }
}