package ulang.calculus

import scala.annotation.tailrec

import ulang._
import ulang.syntax._

object Simplify extends Rule {
  def name = "simplify"
  
  val rw: Rewrites = Map.empty

  def simp(phi: Expr, prove: Boolean, ctx: Goal): Expr = {
    phi match {
      case True | False  => phi
      case Not(phi)      => not(phi, prove, ctx)
      case And(phi, psi) => and(phi, psi, prove, ctx)
      case Or(phi, psi)  => or(phi, psi, prove, ctx)
      case Imp(phi, psi) => imp(phi, psi, prove, ctx)
      case Eqv(phi, psi) => eqv(phi, psi, prove, ctx)
      case Eq(lhs, rhs)  => eq(lhs, rhs, prove, ctx)
      case _             => literal(phi, prove, ctx)
    }
  }

  def apply(seq: Seq): Proof = {
    val goal = con(seq.phis, Goal.empty)

    goal match {
      case Closed =>
        Step(Nil, seq, this)
      case Open(_, phis) =>
        Step(List(Seq(phis)), seq, this)
    }
  }

  def rewrite(self: Expr, ctx: Goal, rw: Rewrites): Expr = {
    rewrite(self, Nil, ctx, rw)
  }

  @tailrec
  def rewrite(self: Expr, args: List[Expr], ctx: Goal, rw: Rewrites): Expr = self match {
    case Id(name) =>
      rw(name)(args, ctx)
    case Apply(fun, arg) =>
      rewrite(fun, term(arg, ctx) :: args, ctx, rw)
    case _ =>
      self
  }

  @tailrec
  def con(todo: List[Expr], ctx: Goal): Goal = todo match {
    case Nil => ctx.reverse
    case phi :: rest =>
      // may produce duplicate work:
      val newctx = assume(rest, ctx)
      val newphi = simp(phi, false, newctx)
      con(rest, assume(newphi, ctx))
  }

  def literal(phi: Expr, prove: Boolean, ctx: Goal): Expr = {
    val newphi = canon(phi, ctx)

    if (ctx contains newphi) True
    else if (ctx contains Not(newphi)) False
    else newphi
  }

  def term(expr: Expr, ctx: Goal): Expr = {
    canon(expr, ctx)
  }

  def canon(expr: Expr, ctx: Goal): Expr = {
    val res = ctx canon expr
    // println("canon " + expr + " ~> " + res)
    res
  }

  def merge(lhs: Expr, rhs: Expr, ctx: Goal): Goal = {
    // println("merge " + lhs + " ~> " + rhs)
    ctx merge (lhs, rhs)
  }

  def not(phi: Expr, prove: Boolean, ctx: Goal): Expr = {
    triv.not(simp(phi, !prove, ctx))
  }

  def and(phi: Expr, psi: Expr, prove: Boolean, ctx: Goal): Expr = {
    val (newphi, newpsi) = binary(phi, true, prove, psi, true, prove, ctx)
    triv.and(newphi, newpsi)
  }

  def or(phi: Expr, psi: Expr, prove: Boolean, ctx: Goal): Expr = {
    val (newphi, newpsi) = binary(phi, false, prove, psi, false, prove, ctx)
    triv.or(newphi, newpsi)
  }

  def imp(phi: Expr, psi: Expr, prove: Boolean, ctx: Goal): Expr = {
    val (newphi, newpsi) = binary(phi, true, !prove, psi, false, prove, ctx)
    triv.imp(newphi, newpsi)
  }

  def eqv(phi: Expr, psi: Expr, prove: Boolean, ctx: Goal): Expr = {
    val lr = simp(Imp(phi, psi), prove, ctx)
    val rl = simp(Imp(psi, phi), prove, ctx)

    // try to extract con/dis

    (lr, rl) match {
      case (False, _) => False
      case (_, False) => False
      case (True, _)  => rl // already simplified
      case (_, True)  => lr
      case _ =>
        triv.eqv(simp(phi, false, ctx), simp(psi, false, ctx)) // does a lot of work again
    }
  }

  def eq(lhs: Expr, rhs: Expr, prove: Boolean, ctx: Goal): Expr = {
    val newlhs = term(lhs, ctx)
    val newrhs = term(rhs, ctx)
    triv.eq(newlhs, newrhs)
  }

  def binary(
    phi: Expr, phi_pos: Boolean, phi_prove: Boolean,
    psi: Expr, psi_pos: Boolean, psi_prove: Boolean,
    ctx: Goal,
    psi_done: Boolean = false,
    swap: Boolean = false): (Expr, Expr) =
    {
      val newctx = if (psi_pos) assume(psi, ctx) else assert(psi, ctx)
      val newphi = simp(phi, phi_prove, newctx)
      val phi_done = phi == newphi

      if (phi_done && psi_done) {
        if (swap) (psi, phi)
        else (phi, psi)
      } else {
        binary(psi, psi_pos, psi_prove,
          /**/ newphi, phi_pos, phi_prove,
          /**/ ctx, phi_done, !swap)
      }
    }

  def assume(phi: Expr, ctx: Goal): Goal = phi match {
    case True =>
      ctx
    case False =>
      Closed
    case Not(psi) =>
      assert(psi, ctx)
    case And(phi, psi) =>
      assume(phi, assume(psi, ctx))
    case Eq(lhs, rhs) =>
      phi :: merge(lhs, rhs, ctx)
    /*case Ex(bound, body) => // slow
      val avoid = free(phi :: ctx)
      assume(inst(body, fresh(bound, avoid)), ctx)*/
    case _ => phi :: ctx
  }

  def assert(phi: Expr, ctx: Goal): Goal = phi match {
    case True =>
      Closed
    case False =>
      ctx
    case Not(psi) =>
      assume(psi, ctx)
    case Imp(phi, psi) =>
      assert(psi, assume(phi, ctx))
    case Or(phi, psi) =>
      assert(psi, assert(phi, ctx))
    /*case All(bound, body) => // slow
      val avoid = free(phi :: ctx)
      assert(inst(body, fresh(bound, avoid)), ctx)*/
    case _ => triv.not(phi) :: ctx
  }

  def assume(args: List[Expr], ctx: Goal): Goal = {
    args.foldRight(ctx)(assume)
  }

  def assert(args: List[Expr], ctx: Goal): Goal = {
    args.foldRight(ctx)(assert)
  }
}