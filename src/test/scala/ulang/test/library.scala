package ulang.test

object library {
  def main(args: Array[String]) {
    import ulang.shell.shell._

    safe {
      load("base")
    }
  }
}