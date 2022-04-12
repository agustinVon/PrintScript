package interpreter

import ast.Operation
import exceptions.InvalidOperationException
import interpreter.ExpressionResultType.{ExpressionResultType, NUM, STR}
import org.austral.ingsis.printscript.parser.Content

trait OperationSolver {
  def solve(operation:Content[String], expr1:Either[ExpressionResultType, Option[Any]], expr2:Either[ExpressionResultType, Option[Any]]):Either[ExpressionResultType, Option[Any]]
}
