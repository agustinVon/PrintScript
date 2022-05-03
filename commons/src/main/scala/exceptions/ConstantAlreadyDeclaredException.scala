package exceptions

case class ConstantAlreadyDeclaredException(l: Int, c: Int) extends PrintScriptException {
  @Override override def getMessage: String = "Constant"
}
