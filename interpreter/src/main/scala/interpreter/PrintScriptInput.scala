package interpreter

case class PrintScriptInput () extends InputMethod {
  override def readInput() = {
    scala.io.StdIn.readLine()
  }
}

