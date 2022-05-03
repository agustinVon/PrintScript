package exceptions

case class VariableAlreadyDeclaredException(l: Int, c: Int) extends PrintScriptException{
  @Override override def getMessage: String = "Variable"
}
