package sources

case class StringProgramSource(text: String) extends ProgramSource {
  override def getSourceString: String = text
}
