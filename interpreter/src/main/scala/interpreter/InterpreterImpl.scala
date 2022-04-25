package interpreter

import interpreter.ExpressionResultType.{BOOLEAN, ExpressionResultType, NUM, STR}
import ast.{ASTree, BooleanExpression, Declaration, DeclarationAssignation, Expression, IfCodeBlock, IfElseCodeBlock, LiteralBoolean, LiteralNumber, LiteralString, ParenExpression, PrintLn, ReadInput, Root, SumOrMinus, TimesOrDiv, Variable, VariableAssignation}
import exceptions.{ConditionalExpectedException, ConstantAlreadyDeclaredException, ConstantMustBeInitializedWithValueException, ConstantValueCannotBeModifiedException, InvalidOperationException, TypeMismatchException, VariableAlreadyDeclaredException, VariableNotDeclaredException}
import org.austral.ingsis.printscript.parser.Content


case class InterpreterImpl() extends Interpreter {
  type InterpreterResult = Either[ExpressionResultType, Option[Any]]
  private var variableTypes: Map[String, ExpressionResultType] = Map()
  private var constantTypes: Map[String, ExpressionResultType] = Map()
  private var variableValues: Map[String, Option[Any]]         = Map()
  private var constantValues: Map[String, Option[Any]]         = Map()
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
    merge(variableValues, constantValues)
  }

  private def merge(m1:Map[String, Option[Any]], m2:Map[String, Option[Any]]):Map[String, Option[Any]] =
    (m1.keySet ++ m2.keySet).map({ i => (i -> (m1.getOrElse(i, m2.getOrElse(i, None)))) }).toMap

  private def solveAST(ast: ASTree, displayMethod: (String) => Unit): Unit = {
    ast match {
      case x: Expression => solveExpression(x)
      case VariableAssignation(variable, _, expression) => solveVariableAssignation(variable, expression)
      case DeclarationAssignation(declaration, _, expression) => solveDeclarationAssignation(declaration, expression)
      case Root(sentences) => sentences.foreach(t => solveAST(t, displayMethod))
      case PrintLn(_, expression) => solvePrintLn(expression, displayMethod)
      case Declaration(declaration, id, declType) => solveDeclaration(declaration, id, declType)
      case IfElseCodeBlock(condition, ifCodeBlock, elseCodeBlock) => solveIfElseCodeBlock(condition, ifCodeBlock, elseCodeBlock, displayMethod)
      case IfCodeBlock(condition, codeBlock) => solveIfElseCodeBlock(condition, codeBlock, null, displayMethod)
      case ReadInput(function, message) =>
    }
  }

  private def solveIfElseCodeBlock(condition: BooleanExpression, ifCodeBlock: ASTree, elseCodeBlock: ASTree, displayMethod:(String) => Unit): Unit ={
    var line:Int =0
    var column:Int =0
    condition match {
      case LiteralBoolean(value) =>
        line=value.getToken.component4().getStartLine
        column=value.getToken.component4().getStartCol
      case Variable(value) =>
        line=value.getToken.component4().getStartLine
        column=value.getToken.component4().getStartCol
    }
    val exprResult = solveExpression(condition)
    exprResult match {
      case Left(BOOLEAN) =>
        solveAST(ifCodeBlock, displayMethod)
        elseCodeBlock match {
          case null =>
          case _ => solveAST(elseCodeBlock, displayMethod)
        }
      case Left(_) => throw ConditionalExpectedException(line, column)
      case Right(x) => x.get match {
        case true => solveAST(ifCodeBlock, displayMethod)
        case false => elseCodeBlock match {
          case null =>
          case _ => solveAST(elseCodeBlock, displayMethod)
        }
        case _ => throw ConditionalExpectedException(line, column)
      }

    }
  }

  private def solveDeclaration(declaration: Content[String], id: Content[String], declType: Content[String]): Unit = {
    if (declaration.getContent.equals("const")){
      throw ConstantMustBeInitializedWithValueException(declaration.getToken.component4().getStartLine, declaration.getToken.component4().getStartCol)
    }
    if (validationPhase) {
      checkDeclarationRequirements(declaration, id)
    }
    (declType.getContent, validationPhase) match {
        case ("string", true)  => variableTypes = variableTypes + (id.getContent -> STR)
        case ("number", true)  => variableTypes = variableTypes + (id.getContent -> NUM)
        case ("boolean", true)  => variableTypes = variableTypes + (id.getContent -> BOOLEAN)
        case ("string", false) => variableValues = variableValues + (id.getContent -> Some(""))
        case ("number", false) => variableValues = variableValues + (id.getContent -> Some(0))
        case ("boolean", false) => variableValues = variableValues + (id.getContent -> Some(false))
    }
  }

  private def checkDeclarationRequirements(declaration: Content[String], id: Content[String]): Unit ={
    (declaration.getContent.equals("let"), variableTypes.contains(id.getContent), constantTypes.contains(id.getContent)) match {
      case (true, true, _) => throw VariableAlreadyDeclaredException(declaration.getToken.component4().getStartLine, declaration.getToken.component4().getStartCol)
      case (false, true, _) => throw VariableAlreadyDeclaredException(declaration.getToken.component4().getStartLine, declaration.getToken.component4().getStartCol)
      case (false, _, true) => throw ConstantAlreadyDeclaredException(declaration.getToken.component4().getStartLine, declaration.getToken.component4().getStartCol)
      case (true, _, true) => throw ConstantAlreadyDeclaredException(declaration.getToken.component4().getStartLine, declaration.getToken.component4().getStartCol)
      case _ =>
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
    if (validationPhase) {
      checkDeclarationRequirements(declaration.declaration, declaration.id)
    }
    val expressionResult = solveExpression(expression)
    if (validationPhase) checkTypes(expressionResult, declaration.declType.getContent, line, column)
    storeValue(declaration.id.getContent, expressionResult, declaration.declaration.getContent.equals("let"), line, column)
  }

  private def solveVariableAssignation(variable: Variable, expression: Expression): Unit = {
    val line   = variable.value.getToken.component4().getStartLine
    val column = variable.value.getToken.component4().getStartCol
    if (validationPhase && !variableTypes.contains(variable.value.getContent)) {
      if (constantTypes.contains(variable.value.getContent)){
        throw ConstantValueCannotBeModifiedException(line, column)
      }else{
        throw VariableNotDeclaredException(line, column)
      }
    } else {
      val expressionResult = solveExpression(expression)
      if (validationPhase) {
        variableTypes(variable.value.getContent) match {
          case NUM => checkTypes(expressionResult, "number", line, column)
          case STR => checkTypes(expressionResult, "string", line, column)
          case BOOLEAN => checkTypes(expressionResult, "boolean", line, column)
        }
      }
      storeValue(variable.value.getContent, expressionResult, true, line, column)
    }
  }

  private def checkTypes(expressionResult: InterpreterResult, expectedType: String, line: Int, column: Int): Unit = {
    (expressionResult, expectedType) match {
      case (Left(NUM), "number") =>
      case (Left(STR), "string") =>
      case (Left(BOOLEAN), "boolean") =>
      case _                     => throw TypeMismatchException(line, column)
    }
  }

  private def storeValue(variableID: String, expressionResult: InterpreterResult, variable:Boolean, line:Int, column:Int) = {
    if(validationPhase && constantTypes.contains(variableID)){
      throw ConstantValueCannotBeModifiedException(line, column)
    }
    (variable, expressionResult) match {
      case (true, Left(x))=> variableTypes = variableTypes + (variableID -> x)
      case (false, Left(x))=> constantTypes = constantTypes + (variableID -> x)
      case (true, Right(x)) => variableValues = variableValues + (variableID -> x)
      case (false, Right(x)) => constantValues = constantValues + (variableID -> x)
    }
  }

  private def solveExpression(expression: Expression): InterpreterResult = {
    expression match {
      case ParenExpression(expression) => solveExpression(expression)
      case Variable(value) => solveVariable(value)
      case LiteralNumber(value) => solveLiteralNumber(value)
      case LiteralString(value) => solveLiteralString(value)
      case LiteralBoolean(value) =>solveLiteralBoolean(value)
      case SumOrMinus(exp1, operator, exp2) => solveOperation(exp1, operator, exp2)
      case TimesOrDiv(exp1, operator, exp2) => solveOperation(exp1, operator, exp2)
    }
  }

  private def solveVariable(value: Content[String]): InterpreterResult = {
    if (validationPhase) {
      Left(
        variableTypes.getOrElse(value.getContent,
          constantTypes.getOrElse(value.getContent,
            throw VariableNotDeclaredException(
            value.getToken.component4().getStartLine,
            value.getToken.component4().getStartCol
          )
        )
      )
      )
    } else {
      Right(
        variableValues.getOrElse(
          value.getContent,
          constantValues.getOrElse(value.getContent,
          throw VariableNotDeclaredException(
            value.getToken.component4().getStartLine,
            value.getToken.component4().getStartCol
          )
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

  private def solveLiteralBoolean(value: Content[Boolean]): InterpreterResult = {
    if (validationPhase) {
      Left(BOOLEAN)
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
