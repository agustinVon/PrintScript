package exceptions

case class ConstantValueCannotBeModifiedException(l: Int, c: Int) extends PrintScriptException {
  @Override override def getMessage: String = "Constant"
}
