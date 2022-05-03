package interpreter

case class PrintScriptPrinterCatcher() extends DisplayMethod {
  private var txt = "";
  override def display(text: String): Unit = {
    txt += text
  }
  def getPrintResult(): String = {
    txt
  }
}
