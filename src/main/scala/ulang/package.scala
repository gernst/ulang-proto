import java.io.Reader

import arse._
import scala.collection.mutable.ListBuffer
import java.io.StringReader
import java.io.FileReader
import java.io.File

package object ulang {
  case class Ref[A](var get: A) {
    def set(a: A) { get = a }
    def map(f: A => A) { get = f(get) }
  }
}