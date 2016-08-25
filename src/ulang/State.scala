package ulang

import arse._

class State(var languages: List[Language]) extends Recognizer[List[String]] {
  override def toString = languages.mkString("\n\n")

  def parse(ls: List[Language], in0: List[String]): (List[Language], List[String]) = ls match {
    case Nil =>
      (Nil, in0)

    case l :: ls =>
      {
        val (new_l, in1) = l.parser(in0)
        (new_l :: ls, in1)
      } or {
        val (new_ls, in1) = parse(ls, in0)
        (l :: new_ls, in1)
      }
  }

  def apply(in0: List[String]): List[String] = {
    var in = in0
    while(!in.isEmpty) {
      val (_languages, _in) = parse(languages, in)
      languages = _languages;
      if(in == _in) sys.error("could not parse: " + in)
      in = _in;
    }
    in
  }
}