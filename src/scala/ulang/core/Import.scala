package ulang.core

import scala.language.postfixOps
import arse._
import java.io.File

object Import extends ulang.Language {
  import Parser._
  import Recognizer._
  
  import ulang.languages
  
  case class Source(name: String) extends ulang.Source {
    override def toString = "import " + name + ";"
  }

  case class Compiled(name: String, parts: List[ulang.Compiled]) extends ulang.Compiled
  case class Linked(name: String, parts: List[ulang.Linked]) extends ulang.Compiled

  val Ext = ".txt"
  val parser = "import" ~ Source.from(string) ~ ";"

  val all: Parser[List[String], ulang.Source] = parse((in: List[String]) => alt(languages.map(_.parser), in))

  def load(name: String): List[ulang.Source] = {
    val file = new File(name + Ext)
    val in0 = ulang.tokenize(file)
    val (parts, in1) = (all *)(in0)
    parts
  }

  def compile(parts: List[ulang.Source]) = {
    parts.collect {
      case Source(name) =>
        val parts = load(name)
        val compiled = languages.flatMap(_.compile(parts))
        Compiled(name, compiled)
    }
  }

  def link(compiled: Map[ulang.Language, ulang.Compiled]): List[ulang.Linked] = {
    languages.flatMap(_.link(compiled))
  }
}