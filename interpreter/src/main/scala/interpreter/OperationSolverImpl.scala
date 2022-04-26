package interpreter

import exceptions.InvalidOperationException
import interpreter.ExpressionResultType.{BOOLEAN, ExpressionResultType, NUM, STR}
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
      case (Left(_), Left(STR))                             => Left(STR)
      case (Left(STR), Left(_))                             => Left(STR)
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x + y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x + y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x + y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x + y))
      case (Right(Some(x: String)), Right(Some(y)))         => Right(Some(x + y.toString))
      case (Right(Some(x)), Right(Some(y: String)))         => Right(Some(x.toString + y))
      case _ =>
        throw InvalidOperationException(line, column, "sum operation not implemented for the provided data types")
    }
  }

  private def solveSubstractionOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM))                           => Left(NUM)
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x - y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x - y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x - y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x - y))
      case _ =>
        throw InvalidOperationException(
          line,
          column,
          "substraction operation not implemented for the provided data types"
        )
    }
  }

  private def solveMultiplicationOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM))                           => Left(NUM)
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x * y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x * y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x * y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x * y))
      case _ =>
        throw InvalidOperationException(
          line,
          column,
          "multiplication operation not implemented for the provided data types"
        )
    }
  }

  private def solveDivisionOperation(
      expr1: Either[ExpressionResultType, Option[Any]],
      expr2: Either[ExpressionResultType, Option[Any]],
      line: Int,
      column: Int
  ) = {
    (expr1, expr2) match {
      case (Left(NUM), Left(NUM))                           => Left(NUM)
      case (Right(Some(x: Double)), Right(Some(y: Double))) => Right(Some(x / y))
      case (Right(Some(x: Int)), Right(Some(y: Int)))       => Right(Some(x / y))
      case (Right(Some(x: Double)), Right(Some(y: Int)))    => Right(Some(x / y))
      case (Right(Some(x: Int)), Right(Some(y: Double)))    => Right(Some(x / y))
      case _ =>
        throw InvalidOperationException(line, column, "division operation not implemented for the provided data types")

    }
  }
}
