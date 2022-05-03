package exceptions

case class VariableNotDeclaredException(l: Int, c: Int) extends PrintScriptException{
  @Override override def getMessage: String = "Variable"
}
