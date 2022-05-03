package exceptions

case class ConditionalExpectedException(l: Int, c: Int) extends PrintScriptException{
  @Override override def getMessage: String = "Conditional"
}
