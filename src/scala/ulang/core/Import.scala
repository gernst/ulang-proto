package ulang.core

import arse._

import ulang._

case class Import(name: String) extends Source {
  override def toString = "import " + name + ";"
}

object Import extends (String => Import) with Language {
  import Parser._
  import Recognizer._

  val parser = "import" ~ Import.from(string) ~ ";"
  
  def build(parts: List[Source]) = {
    null
  }
}