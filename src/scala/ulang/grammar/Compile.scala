package ulang.grammar

import arse._

import ulang._

class Compile {
  import Parser._
  import Recognizer._

  def isLit(s: String) = {
    s.head == '\'' && s.last == '\''
  }

  var env: Map[String, Parser[List[String], List[core.Expr]]] = Map()

  def compile(rules: List[Rule]) = {
  }

  def compile(closed: Closed): Parser[List[String], List[core.Expr]] = closed match {
    case Id(name) if isLit(name) =>
      lit(name, Nil)
    case Id(name) =>
      Parser.rec(env(name))
    case Rec(alt) =>
      compile(alt)
  }

  def compile(rep: Rep): Parser[List[String], List[core.Expr]] = rep match {
    case Rep(arg, Some("*")) =>
      val q = compile(arg).*
      q map (_.flatten)
    case Rep(arg, Some("+")) =>
      val q = compile(arg).+
      q map (_.flatten)
    case Rep(arg, None) =>
      compile(arg)
  }

  def compile(seq: Seq): Parser[List[String], List[core.Expr]] = {
    val Seq(args) = seq
    val ps = args map compile
    parse { in0: List[String] =>
      val (es, in1) = Parser.seq(ps, in0)
      (es.flatten, in1)
    }
  }

  def compile(attr: Attr): Parser[List[String], List[core.Expr]] = {
    val Attr(arg, fun) = attr
    val p = compile(arg)
    fun match {
      case Some(fun) => p map {
        args => List(core.Apply(fun, args))
      }
      case None => p
    }
  }

  def compile(alt: Alt): Parser[List[String], List[core.Expr]] = {
    val Alt(args) = alt
    val ps = args map compile
    parse { in0: List[String] =>
      Parser.alt(ps, in0)
    }
  }
}