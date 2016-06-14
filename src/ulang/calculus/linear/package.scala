package ulang.calculus

import Math.floor
import scala.annotation.tailrec

package object linear {
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def gcd(as: Iterable[Int]): Int = {
    as reduce gcd
  }

  // approximation of modulo for the Omega test
  def mod_(a: Int, b: Int): Int = {
    /*val m = a % b
    if (m < b / 2) m
    else m - b*/
    val af: Float = a
    val bf: Float = b
    val res = af - bf * floor(af / bf + 0.5)
    res.toInt
  }
}