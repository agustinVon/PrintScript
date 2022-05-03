package exceptions

case class UnsupportedFeatureException(l: Int, c: Int) extends PrintScriptException {
  @Override override def getMessage: String = "Unsupported"
}
