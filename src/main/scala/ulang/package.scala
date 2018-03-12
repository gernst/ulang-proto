package object ulang {
  case class Ref[A](var get: A) {
    def set(a: A) { get = a }
    def map(f: A => A) { get = f(get) }
  }

  def out(obj: Any) {
    Console.out.println(obj)
    Console.out.flush
  }

  def warning(obj: Any) = {
    Console.err.println(obj.toString)
    Console.err.flush
  }

  def error(obj: Any) = {
    sys.error(obj.toString)
  }

  def main(args: Array[String]) {
    ulang.shell.shell.run(args)
  }
}