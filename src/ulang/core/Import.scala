package ulang.core

import arse._

import ulang._
import java.io.File

object Import extends Language {
  import Parser._
  import Recognizer._
  
  def load(name: String) = parse {
    in: List[String] =>
      (this, Language.tokenize(new File(name + ".txt")) ++ in)
  }
  
  val parser = "import" ~ string ~ ";" >> (load _)
}