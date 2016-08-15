package ulang

package object semantics {
  val True = Obj("True", Nil)
  val False = Obj("False", Nil)
  val Undefined = Obj("?", Nil)
  
  type Env = Map[String, Val]
  
  object Env {
    val empty: Env = Map.empty
  }
  
  def equal(obj1: Val, obj2: Val): Boolean = (obj1, obj2) match {
    case (Obj(tag1, args1), Obj(tag2, args2)) =>
      tag1 == tag2 && args1.length == args2.length && (args1,args2).zipped.forall((equal _).tupled)
    case _ =>
      ???
  }
}