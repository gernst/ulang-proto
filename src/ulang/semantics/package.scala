package ulang

package object semantics {
  val True = Obj("True", Nil)
  val False = Obj("False", Nil)
  val Undefined = Obj("?", Nil)
  
  type Env = Map[String, Val]
  
  object Env {
    val empty: Env = Map.empty
  }
}