package ulang

package object expr {
  trait Val extends Pretty
  type Env = Map[String, Val]
}