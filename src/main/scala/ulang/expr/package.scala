package ulang

package object expr {
  trait Val extends Pretty
  trait Data extends Val
  
  type Stack = List[Val]
  type Env = Map[String, Val]
}