package ulang.test

object prover {
  def main(args: Array[String]) {
    import ulang.shell.shell._

    safe {
      load("mini")
    }
  }
}