package parser.exceptions

case class ExpressionExpectedException(line:Int, position:Int) extends Exception
