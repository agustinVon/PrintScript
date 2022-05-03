package interpreter

case class PrintScriptInputTest(textResult: String) extends InputMethod {
  override def readInput(name: String, displayMethod: DisplayMethod): String = {
    displayMethod.display(name)
    textResult
  }
}
