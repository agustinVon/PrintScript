package interpreter

case class PrintScriptInput() extends InputMethod {
  override def readInput(name: String, displayMethod: DisplayMethod) = {
    displayMethod.display(name)
    scala.io.StdIn.readLine()
  }
}
