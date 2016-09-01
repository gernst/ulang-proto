package ulang.core

import arse._

import ulang._

case class Import(name: String) extends Part {
  override def toString = "import " + name + ";"
}

object Import extends (String => Import) with Language {
  import Parser._
  import Recognizer._

  val parser = "import" ~ Import.from(string) ~ ";"
}