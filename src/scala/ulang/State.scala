package ulang

import arse._

/*
class State(var languages: List[Language]) {
  override def toString = languages.mkString("\n\n")

  def parse(ls: List[Language], in0: List[String]): List[String] = ls match {
    case Nil => fail
    case l :: ls => l.parse(in0) or parse(ls, in0)
  }

  def parse(in0: List[String]): List[String] = {
    var in = in0
    while (!in.isEmpty) {
      in = parse(languages, in) or sys.error("could not parse: " + in)
    }
    in
  }
}
*/