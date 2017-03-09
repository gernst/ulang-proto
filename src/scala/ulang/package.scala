import java.io.Reader

import arse._
import scala.collection.mutable.ListBuffer
import java.io.StringReader
import java.io.FileReader
import java.io.File

package object ulang {
  type Val = Any
  
  def group[A, B](xs: List[(A, B)]) = {
    xs.groupBy(_._1).map {
      case (x, ys) => (x, ys.map(_._2))
    }
  }

  case class Ref[A](var get: A) {
    def set(a: A) { get = a }
    def map(f: A => A) { get = f(get) }
  }

  type Env = Map[String, Val]
  type PEnv = Map[String, Parser[List[String], Expr]]
  type Subst = Map[Pat, Pat]
}