package ulang

import java.io.File
import java.io.Reader
import java.io.FileReader

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

import arse._

trait Language {
  def parser: Parser[List[String], Language]
}

object Language {
  def tokenize(file: File): List[String] = {
    tokenize(new FileReader(file))
  }

  def tokenize(reader: Reader): List[String] = {
    val scanner = new Scanner(reader)
    val res = new ListBuffer[String]()
    var tok = scanner.next()
    while (tok != null) {
      res += tok
      tok = scanner.next()
    }
    res.toList
  }
}