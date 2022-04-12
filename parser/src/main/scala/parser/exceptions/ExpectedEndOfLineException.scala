package parser.exceptions

case class ExpectedEndOfLineException(message: String, line: Int, position: Int) extends Exception()
