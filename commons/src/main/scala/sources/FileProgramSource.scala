package sources

import scala.io.BufferedSource

case class FileProgramSource(file: BufferedSource) extends ProgramSource {
  override def getSourceString: String = file.mkString
}
