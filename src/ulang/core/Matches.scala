package ulang.core

import arse._

object Matches {
  def matches(pats: List[Pat], args: List[Val], env: Env): Env = (pats, args) match {
    case (Nil, Nil) =>
      env

    case (pat :: pats, arg :: args) =>
      matches(pats, args, matches(pat, arg, env))

    case _ =>
      fail
  }

  def matches(pat: Pat, arg: Val, env: Env): Env = pat match {
    case Constr(name, pats) =>
      arg match {
        case Obj(`name`, args) =>
          matches(pats, args, env)
        case _ => fail
      }

    case Wildcard =>
      env

    case Id(name) =>
      (env get name) match {
        case Some(`arg`) => env
        case None => env + (name -> arg)
        case _ => fail
      }

    case _ =>
      fail
  }

}