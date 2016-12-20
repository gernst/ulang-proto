package ulang

import java.io.File
import java.io.Reader
import java.io.FileReader

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

import arse._
import java.io.StringReader

trait Source
trait Compiled
trait Linked

trait Language {
  def parser: Parser[List[String], Source]
  def compile(sources: List[Source]): List[Compiled]
  def link(compiled: Map[Language, Compiled]): List[Linked]
}