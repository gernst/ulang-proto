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
    val scanner = new scanner(reader)
    val res = new ListBuffer[String]()
    var tok = scanner.next()
    while (tok != null) {
      res += tok
      tok = scanner.next()
    }
    res.toList
  }
  
    val Wildcard = Id("_")

  val True = Tag("True")
  val False = Tag("False")

  type Val = Any
  type Fun = (List[Val], Env) => Val

  type Env = Map[String, Val]

  object Env {
    val empty: Env = Map.empty
    val default: Env = Map("=" -> builtin_equal _)
  }

  def test(b: Boolean) = if (b) True else False

  def builtin_equal(objs: List[Val], dyn: Env): Val = objs match {
    case List(obj1, obj2) =>
      test(equal(obj1, obj2))
  }

  def equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Id(name1), Id(name2)) =>
      name1 == name2
    case (Obj(data1, args1), Obj(data2, args2)) =>
      if(!equal(data1, data2)) false
      if(args1.length != args2.length) false
      else (args1,args2).zipped.forall((equal _).tupled)
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}