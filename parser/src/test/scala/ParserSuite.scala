import ast.{Expression, LiteralNumber, LiteralString, Operation, Variable, VariableAssignation}
import lexer.{LexerImpl, StringProgramSource}
import org.austral.ingsis.printscript.parser.TokenIterator
import org.junit.jupiter.api.Test
import parser.ParserStrategies.{ExpressionParser, VariableParser}
import parser.{ParserImpl, TokenConsumerImpl}
import tokens.TokenTypesImpl

import scala.jdk.CollectionConverters._

class ParserSuite  {

  private def getConsumer(content:String): TokenConsumerImpl = {
    val source = StringProgramSource(content)
    val tokens = new LexerImpl().lex(source).asJava
    TokenConsumerImpl(TokenIterator.create(content, tokens))
  }

  @Test
  def expressionParserShouldParseLiterals(): Unit = {
    val consumer = getConsumer("\"test\"")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case LiteralString(value) => assert(value.component1().equals("\"test\""))
      case _ => assert(false)
    }
  }

  @Test
  def expressionParserShouldParseVariables(): Unit = {
    val consumer = getConsumer("a")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case Variable(value) => assert(value.component1().equals("a"))
      case _ => assert(false)
    }
  }

  @Test
  def expressionParserShouldParseOperations(): Unit = {
    val consumer = getConsumer("2 + 4")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case Operation(exp1, operator, exp2) => {
        exp1 match {
          case LiteralNumber(value) => assert(value.component1() == 2)
          case _ => assert(false)
        }
        assert(operator.component1().equals("+"))
        exp2 match {
          case LiteralNumber(value) => assert(value.component1() == 4)
          case _ => assert(false)
        }
      }
    }
  }

  @Test
  def expressionParserShouldParseVariableAndLiteralOperations(): Unit = {
    val consumer = getConsumer("a + 4 - 5")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case Operation(exp1, operator, exp2) => {
        exp1 match {
          case Variable(value) => assert(value.component1().equals("a"))
          case _ => assert(false)
        }
        assert(operator.component1().equals("+"))
        exp2 match {
          case Operation(exp3, operator, exp4) => {
            exp3 match {
              case LiteralNumber(value) => assert(value.component1() == 4)
              case _ => assert(false)
            }
            assert(operator.component1().equals("-"))
            exp4 match {
              case LiteralNumber(value) => assert(value.component1() == 5)
              case _ => assert(false)
            }
          }
          case _ => assert(false)
        }
      }
    }
  }

  @Test
  def assignationOfVariableShouldAssign(): Unit = {
    val consumer = getConsumer("a = 4")

    val assignation = VariableParser.parse(consumer)

    assignation match {
      case VariableAssignation(variable, assignation, expression) => {
        assert(variable.value.component1().equals("a"))
        assert(assignation.component1().equals("="))
        expression match {
          case LiteralNumber(value) => assert(value.component1() == 4)
          case _ => assert(false)
        }
      }
    }
  }

  @Test
  def assignationOfSumShouldAssign():Unit = {
    val consumer = getConsumer("a = 4 + 5")

    val assignation = VariableParser.parse(consumer)

    assignation match {
      case VariableAssignation(variable, assignation, expression) => {
        assert(variable.value.component1().equals("a"))
        assert(assignation.component1().equals("="))
        expression match {
          case Operation(exp1, operator, exp2) => {
            exp1 match {
              case LiteralNumber(value) => assert(value.component1() == 4)
            }
            assert(operator.component1().equals("+"))
            exp2 match {
              case LiteralNumber(value) => assert(value.component1() == 5)
            }
          }
        }
      }
    }
  }

  @Test
  def tokensShouldBeParsed():Unit = {
    val content = "let a:string = \"ab\" + \"cd\"; a = \"hello world\";"
    val lexer = new LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = new ParserImpl()

    val ast = parser.parse(content, tokens.asJava)

    println(ast)
  }
}
