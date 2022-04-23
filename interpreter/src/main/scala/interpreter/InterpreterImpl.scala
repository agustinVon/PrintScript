package interpreter

import interpreter.ExpressionResultType.{ExpressionResultType, NUM, STR}
import ast.{ASTree, Declaration, DeclarationAssignation, Expression, LiteralNumber, LiteralString, ParenExpression, PrintLn, Root, SumOrMinus, TimesOrDiv, Variable, VariableAssignation}
import exceptions.{InvalidOperationException, TypeMismatchException, VariableAlreadyDeclaredException, VariableNotDeclaredException}
import org.austral.ingsis.printscript.parser.Content

case class InterpreterImpl() extends Interpreter {
  type InterpreterResult = Either[ExpressionResultType, Option[Any]]
  private var variableTypes: Map[String, ExpressionResultType] = Map()
  private var variableValues: Map[String, Option[Any]]         = Map()
  private var validationPhase: Boolean                         = false
  private val operationSolver: OperationSolver                 = OperationSolverImpl()

  override def interpret(ast: ASTree, displayMethod: (String) => Unit): Unit = {
    validate(ast, displayMethod)
    variableValues = Map()
    solveAST(ast, displayMethod)
  }

  override def validate(ast: ASTree, displayMethod: (String) => Unit): Unit = {
    validationPhase = true
    variableTypes = Map()
    solveAST(ast, displayMethod)
    validationPhase = false
  }

  override def getMemory(): Map[String, Option[Any]] = {
    variableValues
  }

  private def solveAST(ast: ASTree, displayMethod: (String) => Unit): Unit = {
    ast match {
      case x: Expression                                      => solveExpression(x)
      case VariableAssignation(variable, _, expression)       => solveVariableAssignation(variable, expression)
      case DeclarationAssignation(declaration, _, expression) => solveDeclarationAssignation(declaration, expression)
      case Root(sentences)                                    => sentences.foreach(t => solveAST(t, displayMethod))
      case PrintLn(_, expression)                             => solvePrintLn(expression, displayMethod)
      case Declaration(declaration, id, declType)             => solveDeclaration(declaration, id, declType)
    }
  }

  private def solveDeclaration(declaration: Content[String], id: Content[String], declType: Content[String]): Unit = {
    if (validationPhase && variableTypes.contains(id.getContent)) {
      throw VariableAlreadyDeclaredException(
        declaration.getToken.component4().getStartLine,
        declaration.getToken.component4().getStartCol
      )
    } else {
      (declType.getContent, validationPhase) match {
        case ("string", true)  => variableTypes = variableTypes + (id.getContent -> STR)
        case ("number", true)  => variableTypes = variableTypes + (id.getContent -> NUM)
        case ("string", false) => variableValues = variableValues + (id.getContent -> Some(""))
        case ("number", false) => variableValues = variableValues + (id.getContent -> Some(0))
      }
    }
  }

  private def solvePrintLn(expression: Expression, displayMethod: (String) => Unit): Unit = {
    solveExpression(expression) match {
      case Left(_)  =>
      case Right(x) => displayMethod(x.get.toString)
    }
  }

  private def solveDeclarationAssignation(declaration: Declaration, expression: Expression): Unit = {
    val line   = declaration.declaration.getToken.component4().getStartLine
    val column = declaration.declaration.getToken.component4().getStartCol
    if (validationPhase && variableTypes.contains(declaration.id.getContent)) {
      throw VariableAlreadyDeclaredException(line, column)
    } else {
      val expressionResult = solveExpression(expression)
      if (validationPhase) checkTypes(expressionResult, declaration.declType.getContent, line, column)
      storeValue(declaration.id.getContent, expressionResult)
    }
  }

  private def solveVariableAssignation(variable: Variable, expression: Expression): Unit = {
    val line   = variable.value.getToken.component4().getStartLine
    val column = variable.value.getToken.component4().getStartCol
    if (validationPhase && !variableTypes.contains(variable.value.getContent)) {
      throw VariableNotDeclaredException(line, column)
    } else {
      val expressionResult = solveExpression(expression)
      if (validationPhase) {
        variableTypes(variable.value.getContent) match {
          case NUM => checkTypes(expressionResult, "number", line, column)
          case STR => checkTypes(expressionResult, "string", line, column)
        }
      }
      storeValue(variable.value.getContent, expressionResult)
    }
  }

  private def checkTypes(expressionResult: InterpreterResult, expectedType: String, line: Int, column: Int): Unit = {
    (expressionResult, expectedType) match {
      case (Left(NUM), "number") =>
      case (Left(STR), "string") =>
      case _                     => throw TypeMismatchException(line, column)
    }
  }

  private def storeValue(variableID: String, expressionResult: InterpreterResult) = {
    expressionResult match {
      case Left(x) =>
        variableTypes = variableTypes + (variableID -> x)
      case Right(x) =>
        variableValues = variableValues + (variableID -> x)
    }
  }

  private def solveExpression(expression: Expression): InterpreterResult = {
    expression match {
      case ParenExpression(expression) => solveExpression(expression)
      case Variable(value) => solveVariable(value)
      case LiteralNumber(value) => solveLiteralNumber(value)
      case LiteralString(value) => solveLiteralString(value)
      case SumOrMinus(exp1, operator, exp2) => solveOperation(exp1, operator, exp2)
      case TimesOrDiv(exp1, operator, exp2) => solveOperation(exp1, operator, exp2)
    }
  }

  private def solveVariable(value: Content[String]): InterpreterResult = {
    if (validationPhase) {
      Left(
        variableTypes.getOrElse(
          value.getContent,
          throw VariableNotDeclaredException(
            value.getToken.component4().getStartLine,
            value.getToken.component4().getStartCol
          )
        )
      )
    } else {
      Right(
        variableValues.getOrElse(
          value.getContent,
          throw VariableNotDeclaredException(
            value.getToken.component4().getStartLine,
            value.getToken.component4().getStartCol
          )
        )
      )
    }
  }

  private def solveLiteralNumber(value: Content[Double]): InterpreterResult = {
    if (validationPhase) {
      Left(NUM)
    } else {
      val x      = value.getToken.getTo - value.getToken.getFrom
      val number = value.getContent + ""
      val isInt  = x < number.length
      isInt match {
        case true  => Right(Some(value.getContent.toInt))
        case false => Right(Some(value.getContent))
      }
    }
  }

  private def solveLiteralString(value: Content[String]): InterpreterResult = {
    if (validationPhase) {
      Left(STR)
    } else {
      Right(Some(value.getContent))
    }
  }

  private def solveOperation(exp1: Expression, operator: Content[String], exp2: Expression): InterpreterResult = {
    val t1 = solveExpression(exp1)
    val t2 = solveExpression(exp2)
    operationSolver.solve(operator, t1, t2)
  }
}
