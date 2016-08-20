package ulang

package object core {
  val True = Obj("True", Nil)
  val False = Obj("False", Nil)

  type Env = Map[String, Val]

  def identity[A](a: A) = a

  def isTag(str: String) = {
    str.head.isUpper || Constrs.contains(str)
  }

  def isFun(str: String) = {
    !str.head.isUpper || Funs.contains(str)
  }

  object Env {
    val empty: Env = Map.empty
    val defailt: Env = Map("==" -> builtin_equals)
  }
  
  object builtin_equals extends Prim({
    case List(obj1, obj2) => 
      if(equals(obj1, obj2)) True
      else False
  })

  def equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Obj(tag1, args1), Obj(tag2, args2)) =>
      tag1 == tag2 && args1.length == args2.length && (args1, args2).zipped.forall((equal _).tupled)
    case _ =>
      sys.error("cannot compare " + obj1 + " and " + obj2)
  }
}