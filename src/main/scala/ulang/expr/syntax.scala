package ulang.expr

import ulang.Pretty

sealed trait HasEq

sealed trait Pat extends Pretty {
  def free: List[Free] = this match {
    case Wildcard | _: Lit | _: Tag | _: Bound => List()
    case free: Free => List(free)
    case UnApp(fun, arg) => fun.free ++ arg.free
    case SubPat(bound, pat) => bound :: pat.free
  }

  def anon: Pat = this match {
    case Wildcard => Wildcard
    case _: Free => Free("?")
    case _: Lit | _: Tag | _: Bound => this
    case UnApp(fun, arg) => UnApp(fun.anon, arg.anon)
    case SubPat(bound, pat) => SubPat(???, pat.anon) // cannot put Wildcard, because.
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
    case App(fun, arg) => UnApp(fun.toPat, arg.toPat)
    case _ => ulang.error("not a pattern: " + this)
  }

  def force: Expr = this
  def bind(bound: List[Free], index: Int): Expr
  def unbind(bound: List[Free], index: Int): Expr = ???
}

object Expr {
  def bind(pat: Pat, body: Expr): Expr = {
    val bound = pat.free
    body bind (bound.reverse, 0)
  }

  def bind(pats: List[Pat], body: Expr): Expr = {
    val bound = pats.flatMap(_.free)
    body bind (bound.reverse, 0)
  }
}

sealed trait Atom extends Expr with Pat {
  def bind(bound: List[Free], index: Int): Atom
}

case class Lit(any: Any) extends Atom {
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

case class Tag(name: String) extends Id with HasEq {
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
    case UnApp(fun, arg) =>
      (this in fun) || (this in arg)
    case SubPat(_, pat) =>
      this in pat
  }

  def in(expr: Expr): Boolean = expr match {
    case _: Lit | _: Tag | _: Bound =>
      false
    case that: Free =>
      this == that
    case App(fun, arg) =>
      (this in fun) || (this in arg)
    case Lambda(cases) =>
      cases exists (this in _)
    case MatchWith(arg, cases) =>
      (this in arg) || (cases exists (this in _))
    case IfThenElse(test, iftrue, iffalse) =>
      (this in test) || (this in iftrue) || (this in iffalse)
  }

  def in(cs: Case): Boolean = {
    val Case(pat, body) = cs
    (this in body) && !(this in pat)
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

case class UnApp(fun: Pat, arg: Pat) extends Pat {
  // assert(!args.isEmpty)
  def bind(bound: List[Free], index: Int) = {
    UnApp(fun bind (bound, index), arg bind (bound, index))
  }
}

case class App(fun: Expr, arg: Expr) extends Expr with HasEq {
  //assert(!args.isEmpty)
  def bind(bound: List[Free], index: Int) = {
    App(fun bind (bound, index), arg bind (bound, index))
  }
}

case class Case(pat: Pat, body: Expr) extends Pretty {
  def bind(bound: List[Free], index: Int) = {
    val shift = pat.free.size
    Case(pat, body bind (bound, index + shift))
  }
}

object Case {
  object binding extends ((Pat, Expr) => Case) {
    def apply(_pat: Pat, _body: Expr): Case = {
      val pat = Pat.linear(_pat)
      val body = Expr.bind(pat, _body)
      Case(pat, body)
    }
  }
}

object Cases {
  def bind(bound: List[Free], index: Int, cases: List[Case]) = {
    cases map {
      case Case(pat, body) =>
        val shift = pat.free.size
        Case(pat, body bind (bound, index + shift))
    }
  }
}

case class Lambda(cases: List[Case]) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    Lambda(Cases.bind(bound, index, cases))
  }

  def |(that: Lambda) = {
    Lambda(this.cases ++ that.cases)
  }
}

object Lambda extends (List[Case] => Expr) {
  def singleton(bound: Pat, body: Expr) = {
    Lambda(List(Case(bound, body)))
  }

  object bindings extends (List[(List[Pat], Expr)] => Expr) {
    def apply(cases: List[(List[Pat], Expr)]): Expr = {
      assert(!cases.isEmpty)
      val lambdas = cases map Lambda.binding.tupled
      lambdas reduce (_ | _)
    }
  }

  object binding extends ((List[Pat], Expr) => Lambda) {
    def apply(_pats: List[Pat], _body: Expr): Lambda = {
      val pats = Pat.linear(_pats)
      val body = Expr.bind(pats, _body)
      pats.foldRight(body)(Lambda.singleton).asInstanceOf[Lambda] // XXX somewhat hacky
    }
  }
}

case class MatchWith(arg: Expr, cases: List[Case]) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    MatchWith(arg bind (bound, index), Cases.bind(bound, index, cases))
  }
}

case class IfThenElse(test: Expr, iftrue: Expr, iffalse: Expr) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    IfThenElse(test bind (bound, index), iftrue bind (bound, index), iffalse bind (bound, index))
  }
}

object LetEq extends ((Pat, Expr) => (Pat, Expr)) {
  def apply(pat: Pat, arg: Expr) = (pat, arg)
}

object LetIn extends ((List[(Pat, Expr)], Expr) => Expr) {
  def apply(eqs: List[(Pat, Expr)], body: Expr) = {
    val (pats, args) = eqs.unzip
    val fun = Lambda.binding(pats, body)
    Apps(fun, args)
  }
}

case class Lazy(expr: Expr, lex: Stack) extends Expr {
  def bind(bound: List[Free], index: Int) = {
    ulang.error("cannot bind a deferred value")
  }

  override lazy val force = {
    eval.eval(expr, lex)
  }
}