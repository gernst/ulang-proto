package ulang

import arse._
import ulang._
import java.io.File

case class Clos(cases: List[Case], lex: Env) extends Pretty
case class Prim(name: String, f: List[Val] => Val) extends Pretty
case class Obj(tag: Tag, args: List[Val]) extends Pretty

case class Lazy(body: Expr, lex: Env) extends Pretty {
  var memo: Option[Val] = None
  def getOrElseUpdate(f: => Val) = {
    if (memo == None)
      memo = Some(f)
    memo.get
  }
}

object Env {
  val empty: Env = Map.empty
  val default: Env = Map("=" -> builtin.equal, "print" -> builtin.print)
}

object builtin {
  val True = Tag("True")
  val False = Tag("False")
  def reify(b: Boolean) = if (b) True else False

  def uncons(h: Pat, t: Pat) = UnApp(Tag("::"), List(h, t))
  def cons(h: Expr, t: Expr) = App(Tag("::"), List(h, t))

  def reify_option(e: Option[Expr]) = e match {
    case None => Tag("None")
    case Some(e) => App(Tag("Some"), List(e))
  }

  def reify_list(ps: List[Pat]) = ps.foldRight(Tag("[]"): Pat)(uncons)
  def reify_list(es: List[Expr]) = es.foldRight(Tag("[]"): Expr)(cons)

  def reify_atom(atom: Atom): Expr = atom match {
    case Tag(name) =>
      App(Tag("Tag"), List(Lit(name)))
    case Id(name) =>
      App(Tag("Id"), List(Lit(name)))
  }

  def reify(cs: Case): Expr = cs match {
    case Case(pats, cond, body) =>
      App(Tag("Case"), List(reify_list(pats map reify), reify_option(cond map reify), reify(body)))
  }

  def reify(pat: Pat): Expr = pat match {
    case Wildcard =>
      Tag("_")
    case l: Lit =>
      l
    case atom: Atom =>
      reify_atom(atom)
    case SubPat(name, pat) =>
      App(Tag("SubPat"), List(Lit(name), reify(pat)))
    case Force(pat) =>
      App(Tag("Force"), List(reify(pat)))
    case UnApp(fun, args) =>
      App(Tag("App"), List(reify(fun), reify_list(args map reify)))
  }

  def reify(expr: Expr): Expr = expr match {
    case l: Lit =>
      l
    case atom: Atom =>
      reify_atom(atom)
    case Susp(pat) =>
      App(Tag("Susp"), List(reify(pat)))
    case App(fun, args) =>
      App(Tag("App"), List(reify(fun), reify_list(args map reify)))
    case Bind(cases) =>
      App(Tag("Bind"), List(reify_list(cases map reify)))
    case IfThenElse(test, iftrue, iffalse) =>
      App(Tag("IfThenElse"), List(reify(test), reify(iftrue), reify(iffalse)))
    case LetIn(pat, arg, body) =>
      App(Tag("LetIn"), List(reify(pat), reify(arg), reify(body)))
    case MatchWith(args, cases) =>
      App(Tag("MatchWith"), List(reify_list(args map reify), reify_list(cases map reify)))
  }

  val equal = Prim("=", { case List(obj1, obj2) => reify(_equal(obj1, obj2)) })
  val print = Prim("print", { case List(obj) => println(obj); obj })

  def _equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Lit(any1), Lit(any2)) =>
      any1 == any2
    case (Tag(name1), Tag(name2)) =>
      name1 == name2
    case (Obj(data1, args1), Obj(data2, args2)) =>
      if (!_equal(data1, data2)) false
      if (args1.length != args2.length) false
      else (args1, args2).zipped.forall((_equal _).tupled)
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}

object interpreter {
  def bind(pat: Pat, arg: Val, dyn: Env, env: Env): Env = pat match {
    case Wildcard =>
      env

    case Lit(any) =>
      if (any == arg) env
      else fail

    case id @ Tag(_) =>
      if (id == arg) env
      else fail

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }
      
    case SubPat(name, pat) =>
      bind(pat, arg, dyn, env + (name -> arg))

    case Force(pat) =>
      arg match {
        case arg @ Lazy(body, lex) =>
          val inner = arg.getOrElseUpdate(eval(body, lex, dyn))
          bind(pat, inner, dyn, env)
        case _ =>
          fail
      }

    case UnApp(pfun, parg) =>
      arg match {
        case Obj(vfun, varg) =>
          bind(parg, varg, dyn, bind(pfun, vfun, dyn, env))
        case _ =>
          fail
      }
  }

  def bind(pats: List[Pat], args: List[Val], dyn: Env, env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, dyn, bind(pat, arg, dyn, env))

    case _ =>
      fail

    /*
    case (_, Nil) =>
      sys.error("missing arguments for " + pats.mkString(" "))

    case (Nil, _) =>
      sys.error("extra arguments " + args.mkString(" "))
      */
  }

  def apply(cs: Case, args: List[Val], lex: Env, dyn: Env): Val = cs match {
    case Case(pats, cond, body) =>
      val env = bind(pats, args, dyn, Env.empty)
      val newlex = lex ++ env
      cond.map(eval(_, newlex, dyn)).foreach {
        case builtin.True =>
        case builtin.False => fail
        case res => sys.error("not a condition in pattern: " + res)
      }
      eval(body, newlex, dyn)
  }

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Env): Val = cases match {
    case Nil =>
      fail // sys.error("no case for " + args.mkString(" "))

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(fun: Val, args: List[Val], dyn: Env): Val = fun match {
    case tag: Tag =>
      Obj(tag, args)

    case Clos(cases, lex) =>
      apply(cases, args, lex, dyn) or { sys.error(cases.map(_.pats).mkString(" ") + " mismatches " + args.mkString(" ")) }

    case Prim(_, f) =>
      f(args)

    case _ =>
      sys.error("not a function " + fun)
  }

  def eval(exprs: List[Expr], lex: Env, dyn: Env): List[Val] = {
    exprs map (eval(_, lex, dyn))
  }

  def eval(expr: Expr, lex: Env, dyn: Env): Val = expr match {
    case tag: Tag =>
      tag

    case Lit(any) =>
      any

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn contains name =>
      dyn(name)

    case Id(name) =>
      val bound = lex.keys ++ dyn.keys
      sys.error("unbound identifier " + name + " in " + bound.mkString("[", " ", "]"))

    case LetIn(pat, _arg, body) =>
      val arg = eval(_arg, lex, dyn)
      val env = bind(pat, arg, dyn, lex) or sys.error("cannot bind " + pat + " to " + arg)
      eval(body, env, dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case builtin.True => eval(arg1, lex, dyn)
        case builtin.False => eval(arg2, lex, dyn)
        case res => sys.error("not a condition in test: " + res)
      }

    case Susp(body) =>
      Lazy(body, lex)

    case App(fun, args) =>
      apply(eval(fun, lex, dyn), eval(args, lex, dyn), dyn)

    case MatchWith(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Bind(cases) =>
      Clos(cases, lex)
  }

  def eval(df: Def, lex: Env, dyn: Env): (String, Val) = df match {
    case Def(Id(name), None, rhs) =>
      (name -> eval(rhs, lex, dyn))
  }
}