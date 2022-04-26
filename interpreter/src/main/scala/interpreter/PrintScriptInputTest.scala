package interpreter

case class PrintScriptInputTest(textResult: String) extends InputMethod {
  override def readInput(): String = {
    textResult
  }
}
