package PrintScript.interpreter
import ast._
import org.austral.ingsis.printscript.common.Token
import org.austral.ingsis.printscript.parser.Content

class InterpreterImpl extends Interpreter {
  var variables: Map[String, Option[Any]] = Map()

  override def interpret(ast: ASTree): Unit = {
    interpret2(ast)
  }

  private def interpret2(ast:ASTree):Option[Any] = {
    ast match {
      case Root(sentences) =>
        sentences.foreach(t=>interpret2(t))
        None
      case DeclarationAssignation(declaration, assignation, expression) =>
        val declarationID = declaration.id.component1()
        if (variables.contains(declarationID)){
          error("variable "+declarationID+" already defined", assignation.getToken)
        }else{
          variables = variables + (declaration.id.component1() -> interpret2(expression))
          None
        }


      case x: Expression => getExpressionValue(x)
      case Operation(exp1, operator, exp2) =>getOperationValue(exp1, operator, exp2)
      case PrintLn(function, expression) =>
        println(getExpressionValue(expression).get)
        None
      case VariableAssignation(variable, assignation, expression) =>
        if (variables.contains(variable.value.component1())){
          val v = interpret2(expression)
          variables = variables + (variable.value.component1() -> v)
          v
        }else{
          error("variable '"+ variable.value.component1()+"' not declared.", variable.value.getToken)
        }
    }
  }

  private def getExpressionValue(expression: Expression):Option[Any] = {
    expression match {
      case LiteralNumber(value) => Some(value.component1())
      case LiteralString(value) => Some(value.component1())
      case Variable(value) => variables.getOrElse(value.component1(), error("variable '"+ value.component1()+"' not declared.", value.getToken))
      case Operation(exp1, operator, exp2) => getOperationValue(exp1, operator, exp2)
      case ParenExpression(expression) => getExpressionValue(expression)
    }
  }

  private def getOperationValue(exp1: Expression, operation: Content[String], exp2:Expression):Option[Any] = {
    val v1 = getExpressionValue(exp1)
    val v2 = getExpressionValue(exp2)
    operation.component1() match {
      case "+" => (v1.get, v2.get) match {
        case (x1:Int, x2:Int) => Some(x1 + x2)
        case (x1:Int, x2:String) => Some(x1 + x2)
        case (x1:String, x2:Int) => Some(x1 + x2)
        case (x1:String, x2:String) => Some(x1 + x2)
      }
      case "-" => (v1.get,v2.get) match {
        case (x1:Int, x2:Int) => Some(x1 - x2)
        case (_:Int, _:String) => error("cannot substract a String from an Int", operation.getToken)
        case (_:String, _:Int) => error("cannot substract an Int from a String", operation.getToken)
        case (_:String, _:String) => error("cannot substract two Strings", operation.getToken)
      }
      case "*" => (v1.get,v2.get) match {
        case (x1:Int, x2:Int) => Some(x1 * x2)
        case (_:Int, _:String) => error("cannot multiply a String with an Int", operation.getToken)
        case (_:String, _:Int) => error("cannot multiply an Int with a String", operation.getToken)
        case (_:String, _:String) => error("cannot multiply two Strings", operation.getToken)
      }
      case "/" => (v1.get,v2.get) match {
        case (x1:Int, x2:Int) => Some(x1 / x2)
        case (_:Int, _:String) => error("cannot divide a String with an Int", operation.getToken)
        case (_:String, _:Int) => error("cannot divide an Int with a String", operation.getToken)
        case (_:String, _:String) => error("cannot divide two Strings", operation.getToken)
      }
    }

  }

  private def error(msg:String, token:Token) = {
    println(msg)
    None
  }
}
