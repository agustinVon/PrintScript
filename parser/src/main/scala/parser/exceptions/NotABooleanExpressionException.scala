package parser.exceptions

case class NotABooleanExpressionException(line: Int, position: Int) extends Exception
