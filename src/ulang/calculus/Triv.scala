package ulang.calculus

import ulang._
import ulang.syntax._

object triv {
  def not(phi: Expr): Expr = phi match {
    case True     => False
    case False    => True
    case Not(phi) => phi
    case _        => Not(phi)
  }

  def and(phi: Expr, psi: Expr): Expr = (phi, psi) match {
    case (True, _)       => psi
    case (False, _)      => False
    case (_, True)       => phi
    case (_, False)      => False
    case _ if phi == psi => phi
    case _               => And(phi, psi)
  }

  def or(phi: Expr, psi: Expr): Expr = (phi, psi) match {
    case (True, _)       => True
    case (False, _)      => psi
    case (_, True)       => True
    case (_, False)      => phi
    case _ if phi == psi => phi
    case _               => Or(phi, psi)
  }

  def imp(phi: Expr, psi: Expr): Expr = (phi, psi) match {
    case (True, _)       => psi
    case (False, _)      => True
    case (_, True)       => True
    case _ if phi == psi => True
    case (_, False)      => not(phi)
    // case (_, Imp(psi, chi)) => imp(and(phi, psi), chi)
    // case (Not(phi), Not(psi)) => imp(psi, phi)
    case _               => Imp(phi, psi)
  }

  def eqv(phi: Expr, psi: Expr): Expr = (phi, psi) match {
    case (True, _)            => psi
    case (_, True)            => phi
    case _ if phi == psi      => True
    case _ if Not(phi) == psi => False
    case _ if phi == Not(psi) => False
    case (False, _)           => not(psi)
    case (_, False)           => not(phi)
    case (Not(phi), Not(psi)) => eqv(phi, psi)
    case _                    => Eqv(phi, psi)
  }

  def eq(lhs: Expr, rhs: Expr) = {
    if (lhs == rhs) True
    else Eq(lhs, rhs)
  }

  def ite(test: Expr, arg1: Expr, arg2: Expr) = test match {
    case True  => arg1
    case False => arg2
    case _     => IfThenElse(test, arg1, arg2)
  }
}