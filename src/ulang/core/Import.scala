package ulang.core

import arse._

import ulang._
import java.io.File

case class Import(imports: List[String]) extends Language {
  import Parser._
  import Recognizer._
  
  override def toString = imports.map("import " + _ + ";").mkString("\n")
  
  def load(name: String) = parse {
    in: List[String] =>
      (Import(name :: imports), Language.tokenize(new File(name + ".txt")) ++ in)
  }
  
  val parser = "import" ~ string ~ ";" >> (load _)
}