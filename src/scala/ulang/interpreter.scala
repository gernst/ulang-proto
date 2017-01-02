package ulang

import arse._
import ulang._
import java.io.File

trait Val extends Pretty

case class Clos(cases: List[Case], lex: Env, dyn: Dyn) extends Val
case class Prim(name: String, f: List[Val] => Val) extends Val
case class Obj(tag: Tag, args: List[Val]) extends Val

// imports go to lexical environment
case class State(local: Dyn, imported: Env)

object State {
  def empty = State(Ref(Env.empty), Env.empty)
  def default = State(Ref(Env.empty), Env.default)
}

object Env {
  val empty: Env = Map.empty
  val default: Env = Map("=" -> builtin.equal, "print" -> builtin.print)
}

object builtin {
  def reify(b: Boolean) = if (b) True else False

  val equal = Prim("=", { case List(obj1, obj2) => reify(_equal(obj1, obj2)) })
  val print = Prim("print", { case List(obj) => println(obj); obj })

  def _equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
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
  def bind(pat: Expr, arg: Val, env: Env): Env = pat match {
    case Wildcard =>
      env

    case id @ Tag(_) =>
      if (id == arg) env
      else fail

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }

    case Apply(pfun, parg) =>
      arg match {
        case Obj(vfun, varg) =>
          bind(parg, varg, bind(pfun, vfun, env))
        case _ =>
          fail
      }

    case _ =>
      fail
  }

  def bind(pats: List[Expr], args: List[Val], env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      bind(pats, args, bind(pat, arg, env))
      
    case _ =>
      fail

      /*
    case (_, Nil) =>
      sys.error("missing arguments for " + pats.mkString(" "))

    case (Nil, _) =>
      sys.error("extra arguments " + args.mkString(" "))
      */
  }

  def apply(cs: Case, args: List[Val], lex: Env, dyn: Dyn): Val = cs match {
    case Case(pats, body) =>
      val env = bind(pats, args, Env.empty)
      eval(body, lex ++ env, dyn)
  }

  def apply(cases: List[Case], args: List[Val], lex: Env, dyn: Dyn): Val = cases match {
    case Nil =>
      sys.error("no case for " + args.mkString(" "))

    case cs :: rest =>
      apply(cs, args, lex, dyn) or apply(rest, args, lex, dyn)
  }

  def apply(fun: Val, args: List[Val], dyn: Dyn): Val = fun match {
    case tag: Tag =>
      Obj(tag, args)

    case Clos(cases, lex, dyn) =>
      apply(cases, args, lex, dyn)

    case Prim(_, f) =>
      f(args)

    case _ =>
      sys.error("not a function " + fun)
  }

  def eval(exprs: List[Expr], lex: Env, dyn: Dyn): List[Val] = {
    exprs map (eval(_, lex, dyn))
  }

  def eval(expr: Expr, lex: Env, dyn: Dyn): Val = expr match {
    case tag: Tag =>
      tag

    case Id(name) if lex contains name =>
      lex(name)

    case Id(name) if dyn.get contains name =>
      dyn.get(name)

    case Id(name) =>
      val bound = lex.keys ++ dyn.get.keys
      sys.error("unbound identifier " + name + " in " + bound.mkString("[", " ", "]"))

    case LetIn(pat, _arg, body) =>
      val arg = eval(_arg, lex, dyn)
      val env = bind(pat, arg, lex) or sys.error("cannot bind " + pat + " to " + arg)
      eval(body, env, dyn)

    case IfThenElse(test, arg1, arg2) =>
      eval(test, lex, dyn) match {
        case True => eval(arg1, lex, dyn)
        case False => eval(arg2, lex, dyn)
        case res => sys.error("not a boolean value: " + res)
      }

    case Apply(fun, args) =>
      apply(eval(fun, lex, dyn), eval(args, lex, dyn), dyn)

    case Match(args, cases) =>
      apply(cases, eval(args, lex, dyn), lex, dyn)

    case Bind(cases) =>
      Clos(cases, lex, dyn)
  }

  def eval(df: Def, lex: Env, dyn: Dyn): (String, Val) = df match {
    case Def(Id(name), rhs) =>
      (name -> eval(rhs, lex, dyn))
  }

  def add(cmd: Cmd, st: State): State = cmd match {
    case Imports(names) =>
      import parser._

      val State(local, in) = st

      val out = names.foldLeft(in) {
        (dyn, name) =>
          val mod = parse(grammar.module, new File("src/ulang/" + name + ".txt"))
          val st = add(mod, State.default)
          dyn ++ st.local.get
      }

      State(local, out)

    case Defs(defs) =>
      val funs = defs.collect {
        case Def(Apply(id: Id, args), rhs) if !args.isEmpty =>
          (id, Case(args, rhs))
      }

      val merged = group(funs).map {
        case (id, cases) =>
          Def(id, Bind(cases))
      }

      val consts = defs.collect {
        case df @ Def(_: Id, rhs) =>
          df
      }

      val State(local, imported) = st

      for (df <- merged ++ consts) {
        local.map(_ + eval(df, imported, local))
      }
      
      st
      
    case Evals(exprs) =>
      val State(local, imported) = st
      
      for(expr <- exprs) {
        println(expr)
        print("  == ")
        print(eval(expr, imported, local))
        println(";")
      }
      
      st
  }

  def add(mod: Module, st: State): State = mod match {
    case Module(cmds) =>
      cmds.foldLeft(st) {
        (st, cmd) => add(cmd, st)
      }
  }
}