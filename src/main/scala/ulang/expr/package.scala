package ulang

package object expr {
  trait Val extends Pretty
  
  type Stack = List[Val]
  type Env = Map[String, Val]
}