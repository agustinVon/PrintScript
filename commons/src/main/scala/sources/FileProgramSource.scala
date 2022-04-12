package sources

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Using}

case class FileProgramSource(path: String) extends ProgramSource {
  override def getSourceString: String =
    Using(Source.fromFile(path)) { source => source.mkString } match {
      case Failure(exception) => throw exception
      case Success(value)     => value
    }
}
