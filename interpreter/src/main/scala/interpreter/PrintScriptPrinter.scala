package interpreter

case class PrintScriptPrinter() extends DisplayMethod {
  override def display(text: String): Unit = {
    println(text)
  }
}
