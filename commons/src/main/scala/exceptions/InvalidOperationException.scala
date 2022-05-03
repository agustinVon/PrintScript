package exceptions

case class InvalidOperationException(l: Int, c: Int, msg: String) extends PrintScriptException {
  @Override override def getMessage: String = "Invalid"
}
