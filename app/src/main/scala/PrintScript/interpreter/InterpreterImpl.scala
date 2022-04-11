package PrintScript.interpreter


import PrintScript.interpreter.ExpressionResultType.{ExpressionResult, NUM, STR}
import ast.{ASTree, Declaration, DeclarationAssignation, Expression, LiteralNumber, LiteralString, Operation, ParenExpression, PrintLn, Root, Variable, VariableAssignation}
import exceptions.{InvalidOperationException, TypeMismatchException, VariableAlreadyDeclaredException, VariableNotDeclaredException}
import org.austral.ingsis.printscript.parser.Content

class InterpreterImpl extends Interpreter {
  var variableTypes: Map[String, ExpressionResult] = Map()
  var variableValues: Map[String, Option[Any]] = Map()
  var validationMode: Boolean = false

  override def interpret(ast: ASTree): Unit = {
    validationMode = false
    variableValues = Map()
    solveAST(ast)
  }

  override def validate(ast:ASTree): Unit = {
    validationMode = true
    variableTypes = Map()
    solveAST(ast)
    println("Validation successful")
  }

  override def getMemory():Map[String, Option[Any]] = {
    variableValues
  }

  def solveAST(ast:ASTree):Unit = {
    ast match {
      case x: Expression => solveExpression(x)
      case VariableAssignation(variable, _, expression) => solveVariableAssignation(variable, expression)
      case DeclarationAssignation(declaration, _, expression) => solveDeclarationAssignation(declaration, expression)
      case Root(sentences) => sentences.foreach(t=>solveAST(t))
      case PrintLn(_, expression) => solvePrintLn(expression)
    }
  }

  private def solvePrintLn(expression: Expression): Unit = {
    solveExpression(expression) match {
      case Left(_) =>
      case Right(x) => println(x.get)
    }
  }

  private def solveDeclarationAssignation(declaration: Declaration,expression: Expression): Unit = {
    val line = declaration.declaration.getToken.component4().getStartLine
    val column = declaration.declaration.getToken.component4().getStartCol

    if ((validationMode && variableTypes.contains(declaration.id.getContent)) || variableValues.contains(declaration.id.getContent)) {
      throw VariableAlreadyDeclaredException(line, column)
    }else{
      val expressionResult = solveExpression(expression)
      checkTypes(expressionResult, declaration.declType.getContent, line, column)
      storeValue(declaration.id.getContent, expressionResult)
    }
  }

  private def solveVariableAssignation(variable: Variable, expression: Expression) = {
    val line = variable.value.getToken.component4().getStartLine
    val column = variable.value.getToken.component4().getStartCol
    if ((validationMode && !variableTypes.contains(variable.value.getContent)) || !variableValues.contains(variable.value.getContent)) {
      throw VariableNotDeclaredException(line, column)
    }else{
      val expressionResult = solveExpression(expression)
      if (validationMode){
        variableTypes(variable.value.getContent) match {
          case NUM => checkTypes(expressionResult, "number", line, column)
          case STR => checkTypes(expressionResult, "string", line, column)
        }
      }else{
        variableValues(variable.value.getContent).get match {
          case _:Int => checkTypes(expressionResult, "number", line, column)
          case _:String => checkTypes(expressionResult, "string", line, column)
        }
      }
      storeValue(variable.value.getContent,expressionResult)
    }
  }

  private def checkTypes(expressionResult:Either[ExpressionResult, Option[Any]], expectedType:String, line: Int, column: Int): Unit = {
    (expressionResult, expectedType) match {
      case (Left(NUM), "string") => throw TypeMismatchException(line, column)
      case (Left(STR), "number") => throw TypeMismatchException(line, column)
      case (Right(x), y) =>
        (x.get, y) match {
          case (_: Int, "string") => throw TypeMismatchException(line, column)
          case (_: String, "number") => throw TypeMismatchException(line, column)
          case _ =>
        }
      case _ =>
    }
  }

  private def storeValue(variableID:String, expressionResult: Either[ExpressionResult, Option[Any]]) = {
    expressionResult match {
      case Left(x) =>
        variableTypes = variableTypes + (variableID -> x)
      case Right(x) =>
        variableValues = variableValues + (variableID -> x)
    }
  }

  private def solveExpression(expression: Expression):Either[ExpressionResult, Option[Any]] = {
    expression match {
      case Operation(exp1, operator, exp2) => solveOperation(exp1, operator, exp2)
      case ParenExpression(expression) => solveExpression(expression)
      case Variable(value) => solveVariable(value)
      case LiteralNumber(value) => solveLiteralNumber(value)
      case LiteralString(value) => solveLiteralString(value)
    }
  }

  private def solveVariable(value:Content[String]) = {
    if(validationMode){
      Left(variableTypes.getOrElse(value.getContent,throw VariableNotDeclaredException(value.getToken.component4().getStartLine, value.getToken.component4().getStartCol)))
    }else{
      Right(variableValues.getOrElse(value.getContent,throw VariableNotDeclaredException(value.getToken.component4().getStartLine, value.getToken.component4().getStartCol)))
    }
  }

  private def solveLiteralNumber(value:Content[Int]) = {
    if(validationMode){
      Left(NUM)
    }else{
      Right(Some(value.getContent))
    }
  }

  private def solveLiteralString(value:Content[String]) = {
    if(validationMode){
      Left(STR)
    }else{
      Right(Some(value.getContent))
    }
  }

  private def solveOperation(exp1: Expression, operator: Content[String], exp2: Expression):Either[ExpressionResult, Option[Any]] = {
    val t1 = solveExpression(exp1)
    val t2 = solveExpression(exp2)
    val line = operator.getToken.component4().getStartLine
    val column = operator.getToken.component4().getStartCol

    operator.component1() match {
      case "+" => (t1, t2) match {
        case (Left(NUM), Left(NUM)) => Left(NUM)
        case (Left(NUM), Left(STR)) => Left(STR)
        case (Left(STR), Left(NUM)) => Left(STR)
        case (Left(STR), Left(STR)) => Left(STR)
        case (Right(Some(x:Int)), Right(Some(y:Int))) => Right(Some(x+y))
        case (Right(Some(x:Int)), Right(Some(y:String))) => Right(Some(x+y))
        case (Right(Some(x:String)), Right(Some(y:Int))) => Right(Some(x+y))
        case (Right(Some(x:String)), Right(Some(y:String))) => Right(Some(x+y))
      }
      case "-" => (t1,t2) match {
        case (Left(NUM), Left(NUM)) => Left(NUM)
        case (Left(NUM), Left(STR)) => throw InvalidOperationException(line,column, "cannot substract a string with a number" )
        case (Left(STR), Left(NUM)) => throw InvalidOperationException(line,column, "cannot substract a string with a number" )
        case (Left(STR), Left(STR)) => throw InvalidOperationException(line,column, "cannot substract two strings" )
        case (Right(Some(x:Int)), Right(Some(y:Int))) => Right(Some(x-y))
        case (Right(Some(_:Int)), Right(Some(_:String))) => throw InvalidOperationException(line,column, "cannot substract a string with a number" )
        case (Right(Some(_:String)), Right(Some(_:Int))) => throw InvalidOperationException(line,column, "cannot substract a string with a number" )
        case (Right(Some(_:String)), Right(Some(_:String))) => throw InvalidOperationException(line,column, "cannot substract two strings" )
      }
      case "*" => (t1,t2) match {
        case (Left(NUM), Left(NUM)) => Left(NUM)
        case (Left(NUM), Left(STR)) => throw InvalidOperationException(line,column, "cannot multiply a string with a number" )
        case (Left(STR), Left(NUM)) => throw InvalidOperationException(line,column, "cannot multiply a string with a number" )
        case (Left(STR), Left(STR)) => throw InvalidOperationException(line,column, "cannot multiply two strings" )
        case (Right(Some(x:Int)), Right(Some(y:Int))) => Right(Some(x*y))
        case (Right(Some(_:Int)), Right(Some(_:String))) => throw InvalidOperationException(line,column, "cannot multiply a string with a number" )
        case (Right(Some(_:String)), Right(Some(_:Int))) => throw InvalidOperationException(line,column, "cannot multiply a string with a number" )
        case (Right(Some(_:String)), Right(Some(_:String))) => throw InvalidOperationException(line,column, "cannot multiply two strings" )
      }
      case "/" => (t1,t2) match {
        case (Left(NUM), Left(NUM)) => Left(NUM)
        case (Left(NUM), Left(STR)) => throw InvalidOperationException(line,column, "cannot divide a string with a number" )
        case (Left(STR), Left(NUM)) => throw InvalidOperationException(line,column, "cannot divide a string with a number" )
        case (Left(STR), Left(STR)) => throw InvalidOperationException(line,column, "cannot divide two strings" )
        case (Right(Some(x:Int)), Right(Some(y:Int))) => Right(Some(x/y))
        case (Right(Some(_:Int)), Right(Some(_:String))) => throw InvalidOperationException(line,column, "cannot divide a string with a number" )
        case (Right(Some(_:String)), Right(Some(_:Int))) => throw InvalidOperationException(line,column, "cannot divide a string with a number" )
        case (Right(Some(_:String)), Right(Some(_:String))) => throw InvalidOperationException(line,column, "cannot divide two strings" )
      }
    }
  }
}
