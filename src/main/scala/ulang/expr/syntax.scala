package ulang.expr

import ulang.Pretty

sealed trait Pat extends Pretty {
  def free: List[Free] = this match {
    case Wildcard | _: Lit | _: Tag => List()
    case free: Free => List(free)
    case UnApp(fun, args) => fun.free ++ args.flatMap(_.free)
    case SubPat(_, pat) => pat.free
  }
}

sealed trait Expr extends Pretty {
  def toPat: Pat = this match {
    case atom: Atom => atom
    case App(fun, args) => UnApp(fun.toPat, args map (_.toPat))
    case _ => ulang.error("not a pattern: " + this)
  }

  def bind(bound: List[Free], index: Int): Expr = this match {
    case _: Bound | _: Lit | _: Tag =>
      this

    case free: Free =>
      val i = bound indexOf free
      if (i < 0) this else Bound(index + i)

    case App(fun, args) =>
      App(fun bind (bound, index), args map (_ bind (bound, index)))

    case Lambda(cases) =>
      Lambda(cases map {
        case Case(pats, cond, body) =>
          val shift = pats.flatMap(_.free).distinct.size
          Case(pats, cond.map(_ bind (bound, index + shift)), body bind (bound, index + shift))
      })

    case MatchWith(args, cases) =>
      MatchWith(args map (_ bind (bound, index)), cases map {
        case Case(pats, cond, body) =>
          val shift = pats.flatMap(_.free).distinct.size
          Case(pats, cond.map(_ bind (bound, index + shift)), body bind (bound, index + shift))
      })

    case LetIn(eqs, body) =>
      val pats = eqs map (_.pat)
      val shift = pats.flatMap(_.free).distinct.size
      LetIn(eqs map {
        case LetEq(pat, arg) => LetEq(pat, arg.bind(bound, index))
      }, body bind (bound, index + shift))

    case IfThenElse(test, iftrue, iffalse) =>
      IfThenElse(test bind (bound, index), iftrue bind (bound, index), iffalse bind (bound, index))
  }

  def bind(pat: Pat): Expr = {
    val bound = pat.free.distinct
    bind(bound, 0)
  }

  def bind(pats: List[Pat]): Expr = {
    val bound = pats.flatMap(_.free).distinct
    bind(bound, 0)
  }

  def unbind(bound: List[Free], index: Int): Expr = ???
}

case class Lit(any: Any) extends Expr with Pat with Eq

sealed trait Atom extends Expr with Pat { def name: String }
case class Tag(name: String) extends Atom with Eq

case class Free(name: String) extends Atom {
  def in(pat: Pat): Boolean = pat match {
    case Wildcard | _: Lit | _: Tag =>
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
    case LetIn(eqs, body) =>
      (eqs exists (this in _.arg)) || (this in body) && !(eqs exists (this in _.pat))
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

case object Wildcard extends Pat
case class Bound(index: Int) extends Expr

case class SubPat(name: String, pat: Pat) extends Pat
case class UnApp(fun: Pat, args: List[Pat]) extends Pat // { assert(!args.isEmpty) }
case class App(fun: Expr, args: List[Expr]) extends Expr // { assert(!args.isEmpty) }

case class Case(pats: List[Pat], cond: Option[Expr], body: Expr) extends Pretty

object Case {
  object bind extends ((List[Pat], Option[Expr], Expr) => Case) {
    def apply(pats: List[Pat], cond: Option[Expr], body: Expr): Case = {
      Case(pats, cond map (_ bind pats), body bind pats)
    }
  }
}

case class Lambda(cases: List[Case]) extends Expr

case class MatchWith(args: List[Expr], cases: List[Case]) extends Expr

case class LetEq(pat: Pat, arg: Expr) extends Pretty
case class LetIn(eqs: List[LetEq], body: Expr) extends Expr

object LetIn {
  object bind extends ((List[LetEq], Expr) => Expr) {
    def apply(eqs: List[LetEq], body: Expr): Expr = {
      val pats = eqs.map(_.pat)
      LetIn(eqs, body bind pats)
    }
  }
}

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr

object Atom extends (String => Atom) {
  def isTag(name: String) = {
    name.head.isUpper || operators.data.exists(_.name == name)
  }

  def apply(name: String) = {
    if (isTag(name))
      Tag(name)
    else
      Free(name)
  }

  def unapply(atom: Atom) = {
    Some(atom.name)
  }
}