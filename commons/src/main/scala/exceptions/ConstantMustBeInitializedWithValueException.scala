package exceptions

case class ConstantMustBeInitializedWithValueException(l: Int, c: Int) extends PrintScriptException {
  @Override override def getMessage: String = "Constant"
}
