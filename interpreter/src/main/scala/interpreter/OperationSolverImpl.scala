package interpreter

import exceptions.InvalidOperationException
import interpreter.ExpressionResultType.{ExpressionResultType, NUM, STR}
import org.austral.ingsis.printscript.parser.Content

case class OperationSolverImpl() extends OperationSolver {

  override def solve(
      operation: Content[String],
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]]
  ): Either[ExpressionResultType, Option[Any]] = {
    val line   = operation.getToken.component4().getStartLine
    val column = operation.getToken.component4().getStartCol
    operation.component1() match {
      case "+" => solveSumOperation(expr1, expr2, line, column)
      case "-" => solveSubstractionOperation(expr1, expr2, line, column)
      case "*" => solveMultiplicationOperation(expr1, expr2, line, column)
      case "/" => solveDivisionOperation(expr1, expr2, line, column)
    }
  }

  private def solveSumOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM))                           => Left(NUM)
      case (Left(NUM), Left(STR))                           => Left(STR)
      case (Left(STR), Left(NUM))                           => Left(STR)
      case (Left(STR), Left(STR))                           => Left(STR)
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x + y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x + y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x + y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x + y))
      case (Right(Some(x: Double)), Right(Some(y: String))) => Right(Some(x + y))
      case (Right(Some(x: Int)), Right(Some(y: String)))    => Right(Some(x + y))
      case (Right(Some(x: String)), Right(Some(y: Double))) => Right(Some(x + y))
      case (Right(Some(x: String)), Right(Some(y: Int)))    => Right(Some(x + y))
      case (Right(Some(x: String)), Right(Some(y: String))) => Right(Some(x + y))
    }
  }

  private def solveSubstractionOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM)) => Left(NUM)
      case (Left(NUM), Left(STR)) =>
        throw InvalidOperationException(line, column, "cannot substract a string with a number")
      case (Left(STR), Left(NUM)) =>
        throw InvalidOperationException(line, column, "cannot substract a string with a number")
      case (Left(STR), Left(STR))                           => throw InvalidOperationException(line, column, "cannot substract two strings")
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x - y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x - y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x - y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x - y))
    }
  }

  private def solveMultiplicationOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM)) => Left(NUM)
      case (Left(NUM), Left(STR)) =>
        throw InvalidOperationException(line, column, "cannot multiply a string with a number")
      case (Left(STR), Left(NUM)) =>
        throw InvalidOperationException(line, column, "cannot multiply a string with a number")
      case (Left(STR), Left(STR))                           => throw InvalidOperationException(line, column, "cannot multiply two strings")
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x * y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x * y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x * y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x * y))
    }
  }

  private def solveDivisionOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM)) => Left(NUM)
      case (Left(NUM), Left(STR)) =>
        throw InvalidOperationException(line, column, "cannot divide a string with a number")
      case (Left(STR), Left(NUM)) =>
        throw InvalidOperationException(line, column, "cannot divide a string with a number")
      case (Left(STR), Left(STR))                           => throw InvalidOperationException(line, column, "cannot divide two strings")
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x / y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x / y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x / y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x / y))
    }
  }
}
