package ulang.expr

import ulang.Pretty

sealed trait Pat extends Pretty {
  def free: List[Free] = this match {
    case Wildcard | _: Lit | _: Tag | _: Bound => List()
    case free: Free => List(free)
    case UnApp(fun, args) => fun.free ++ args.flatMap(_.free)
    case SubPat(bound, pat) => bound :: pat.free
  }

  def bind(bound: List[Free], index: Int): Pat
}

object Pat {
  def linear(pat: Pat): Pat = {
    pat bind (List.empty, 0)
  }

  def linear(pats: List[Pat]): List[Pat] = {
    bind(pats, List.empty, 0)
  }

  def bind(pats: List[Pat], bound: List[Free], index: Int): List[Pat] = pats match {
    case Nil =>
      Nil
    case first :: rest =>
      val pat = first bind (bound, index)
      val pats = bind(rest, pat.free ++ bound, index)
      pat :: pats
  }
}

sealed trait Expr extends Pretty {
  def toPat: Pat = this match {
    case atom: Id => atom
    case App(fun, args) => UnApp(fun.toPat, args map (_.toPat))
    case _ => ulang.error("not a pattern: " + this)
  }

  def bind(bound: List[Free], index: Int): Expr
  def unbind(bound: List[Free], index: Int): Expr = ???
}

object Expr {
  def bind(pats: List[Pat], body: Expr): Expr = {
    val bound = pats.flatMap(_.free)
    body bind (bound.reverse, 0)
  }

  def bind(pats: List[Pat], body: Option[Expr]): Option[Expr] = {
    val bound = pats.flatMap(_.free)
    body map (_ bind (bound.reverse, 0))
  }
}

sealed trait Atom extends Expr with Pat {
  def bind(bound: List[Free], index: Int): Atom
}

case class Lit(any: Any) extends Atom with Val with Eq {
  def bind(bound: List[Free], index: Int) = this
}

sealed trait Id extends Atom {
  def name: String
}

object Id extends (String => Id) {
  def isTag(name: String) = {
    name.head.isUpper || operators.data.exists(_.name == name)
  }

  def apply(name: String) = {
    if (isTag(name))
      Tag(name)
    else
      Free(name)
  }

  def unapply(atom: Id) = {
    Some(atom.name)
  }
}

case class Tag(name: String) extends Id with Val with Eq {
  def bind(bound: List[Free], index: Int) = this
}

case class Free(name: String) extends Id {
  def bind(bound: List[Free], index: Int) = {
    val i = bound indexOf this
    if (i < 0) this else Bound(index + i)
  }

  def in(pat: Pat): Boolean = pat match {
    case Wildcard | _: Lit | _: Tag | _: Bound =>
      false
    case that: Free =>
      this == that
    case UnApp(fun, args) =>
      (this in fun) || (args exists (this in _))
    case SubPat(_, pat) =>
      this in pat
  }

  def in(expr: Expr): Boolean = expr match {
    case _: Lit | _: Tag | _: Bound =>
      false
    case that: Free =>
      this == that
    case App(fun, args) =>
      (this in fun) || (args exists (this in _))
    case Lambda(cases) =>
      cases exists (this in _)
    case MatchWith(args, cases) =>
      (args exists (this in _)) || (cases exists (this in _))
    case IfThenElse(test, iftrue, iffalse) =>
      (this in test) || (this in iftrue) || (this in iffalse)
  }

  def in(cs: Case): Boolean = {
    val Case(pats, cond, body) = cs
    val free = (this in body) || (cond exists (this in _))
    val bound = (pats exists (this in _))
    free && !bound
  }
}

case object Wildcard extends Pat {
  def bind(bound: List[Free], index: Int) = this
}

case class Bound(index: Int) extends Atom {
  def bind(bound: List[Free], index: Int) = this
}

case class SubPat(name: Free, pat: Pat) extends Pat {
  def bind(bound: List[Free], index: Int) = {
    SubPat(name, pat bind (name :: bound, index))
  }
}

case class UnApp(fun: Pat, args: List[Pat]) extends Pat {
  // assert(!args.isEmpty)
  def bind(bound: List[Free], index: Int) = {
    val _fun = fun bind (bound, index)
    val _args = args map (_ bind (bound, index))
    UnApp(_fun, _args)
  }
}

case class App(fun: Expr, args: List[Expr]) extends Expr {
  //assert(!args.isEmpty)
  def bind(bound: List[Free], index: Int) = {
    App(fun bind (bound, index), args map (_ bind (bound, index)))
  }
}

case class Case(pats: List[Pat], cond: Option[Expr], body: Expr) extends Pretty {
  def bind(bound: List[Free], index: Int) = {
    val shift = pats.flatMap(_.free).size
    Case(pats, cond.map(_ bind (bound, index + shift)), body bind (bound, index + shift))
  }
}

object Case {
  object binding extends ((List[Pat], Option[Expr], Expr) => Case) {
    def apply(_pats: List[Pat], _cond: Option[Expr], _body: Expr): Case = {
      val pats = Pat.linear(_pats)
      val cond = Expr.bind(pats, _cond)
      val body = Expr.bind(pats, _body)
      Case(pats, cond, body)
    }
  }
}

object Cases {
  def bind(bound: List[Free], index: Int, cases: List[Case]) = {
    cases map {
      case Case(pats, cond, body) =>
        val shift = pats.flatMap(_.free).size
        Case(pats, cond.map(_ bind (bound, index + shift)), body bind (bound, index + shift))
    }
  }
}

case class Lambda(cases: List[Case]) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    Lambda(Cases.bind(bound, index, cases))
  }
}

case class MatchWith(args: List[Expr], cases: List[Case]) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    MatchWith(args map (_ bind (bound, index)), Cases.bind(bound, index, cases))
  }
}

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    IfThenElse(test bind (bound, index), iftrue bind (bound, index), iffalse bind (bound, index))
  }
}
