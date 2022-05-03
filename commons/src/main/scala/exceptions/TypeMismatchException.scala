package exceptions

case class TypeMismatchException(line: Int, Column: Int) extends PrintScriptException{
  @Override override def getMessage: String = "Invalid"
}
