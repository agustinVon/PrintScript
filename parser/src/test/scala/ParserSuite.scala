import ast.{Declaration, DeclarationAssignation, Expression, LiteralNumber, LiteralString, Operation, ParenExpression, PrintLn, Root, Variable, VariableAssignation}
import lexer.{LexerImpl, StringProgramSource}
import org.austral.ingsis.printscript.parser.TokenIterator
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.{DisplayName, Nested, Test}
import parser.ParserStrategies.{DeclarationParser, ExpressionParser, FunctionParser, LiteralParser, VariableParser}
import parser.exceptions.{ExpressionExpectedException, NoStrategyException}
import parser.{ParserImpl, TokenConsumerImpl}

import scala.jdk.CollectionConverters._

class ParserSuite  {
  private def getConsumer(content:String): TokenConsumerImpl = {
    val source = StringProgramSource(content)
    val tokens = LexerImpl().lex(source).asJava
    TokenConsumerImpl(TokenIterator.create(content, tokens))
  }

  @Nested
  @DisplayName("Strategies can parse")
  class CanParse{
    @Test
    def declarationShouldBeAbleToParse():Unit = {
      val consumer = getConsumer("let a:string")

      assert(DeclarationParser.canBeParsed(consumer))
    }

    @Test
    def variableShouldBeAbleToParse():Unit = {
      val consumer = getConsumer("a")

      assert(VariableParser.canBeParsed(consumer))
    }

    @Test
    def literalShouldBeAbleToParse():Unit = {
      val consumer = getConsumer("\"test\"")

      assert(LiteralParser.canBeParsed(consumer))
    }

    @Test
    def functionParserShouldBeAbleToParse():Unit = {
      val consumer = getConsumer("println(\"test\");")

      assert(FunctionParser.canBeParsed(consumer))
    }

    @Test
    def parenExpressionShouldBeAbleToParse():Unit = {
      val consumer = getConsumer("(5 + 2)")

      assert(ExpressionParser.canBeParsed(consumer))
    }
  }

  @Test
  def multipleParenExpressionShouldBeParsed():Unit = {
    val consumer = getConsumer("((5 + 2) * 4)")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case ParenExpression(expression) =>
        expression match {
          case Operation(exp1, operator, exp2) =>
            assert(operator.component1().equals("*"))
            exp1 match {
              case ParenExpression(expression) => {
                expression match {
                  case Operation(exp1, operator, exp2) =>
                    assert(operator.component1().equals("+"))
                }
              }
            }
        }
    }
  }

  @Test
  def expressionParserShouldParseLiterals(): Unit = {
    val consumer = getConsumer("\"test\"")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case LiteralString(value) => assert(value.component1().equals("test"))
      case _ => assert(false)
    }
  }

  @Nested
  @DisplayName("Strategies can parse correctly")
  class ShouldParseCorrectly {
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
        case Operation(exp1, operator, exp2) =>
          exp1 match {
            case LiteralNumber(value) => assert(value.component1() == 2)
            case _ => assert(false)
          }
          assert(operator.component1().equals("+"))
          exp2 match {
            case LiteralNumber(value) => assert(value.component1() == 4)
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    @Test
    def expressionParserShouldParseVariableAndLiteralOperations(): Unit = {
      val consumer = getConsumer("a + 4 - 5")

      val expression = ExpressionParser.parse(consumer)

      expression match {
        case Operation(exp1, operator, exp2) =>
          exp1 match {
            case Variable(value) => assert(value.component1().equals("a"))
            case _ => assert(false)
          }
          assert(operator.component1().equals("+"))
          exp2 match {
            case Operation(exp3, operator, exp4) =>
              exp3 match {
                case LiteralNumber(value) => assert(value.component1() == 4)
                case _ => assert(false)
              }
              assert(operator.component1().equals("-"))
              exp4 match {
                case LiteralNumber(value) => assert(value.component1() == 5)
                case _ => assert(false)
              }
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    @Test
    def assignationOfVariableShouldAssign(): Unit = {
      val consumer = getConsumer("a = 4")

      val assignation = VariableParser.parse(consumer)

      assignation match {
        case VariableAssignation(variable, assignation, expression) =>
          assert(variable.value.component1().equals("a"))
          assert(assignation.component1().equals("="))
          expression match {
            case LiteralNumber(value) => assert(value.component1() == 4)
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    @Test
    def assignationOfSumShouldAssign():Unit = {
      val consumer = getConsumer("a = 4 + 5")

      val assignation = VariableParser.parse(consumer)

      assignation match {
        case VariableAssignation(variable, assignation, expression) =>
          assert(variable.value.component1().equals("a"))
          assert(assignation.component1().equals("="))
          expression match {
            case Operation(exp1, operator, exp2) =>
              exp1 match {
                case LiteralNumber(value) => assert(value.component1() == 4)
                case _ => assert(false)
              }
              assert(operator.component1().equals("+"))
              exp2 match {
                case LiteralNumber(value) => assert(value.component1() == 5)
                case _ => assert(false)
              }
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    @Test
    def declarationShouldBeParsed():Unit = {
      val consumer = getConsumer("let a:string")

      val declarationAssignation = DeclarationParser.parse(consumer)

      declarationAssignation match {
        case Declaration(declaration, id, declType) =>
          assert(declaration.component1().equals("let"))
          assert(declType.component1().equals("string"))
          assert(id.component1().equals("a"))
      }
    }

    @Test
    def declarationAssignationShouldBeParsed():Unit = {
      val consumer = getConsumer("let a:string = \"test\"")

      val declarationAssignation = DeclarationParser.parse(consumer)

      declarationAssignation match {
        case DeclarationAssignation(declaration, assignation, expression) =>
          assert(declaration.declaration.component1().equals("let"))
          assert(declaration.declType.component1().equals("string"))
          assert(declaration.id.component1().equals("a"))
          assert(assignation.component1().equals("="))
          expression match {
            case LiteralString(value) => assert(value.component1().equals("test"))
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    @Test
    def functionParserShouldParse():Unit = {
      val consumer = getConsumer("println(\"hello world\")")

      val function = FunctionParser.parse(consumer)

      function match {
        case PrintLn(function, expression) =>
          function.component1().equals("println")
          expression match {
            case LiteralString(value) => assert(value.component1().equals("hello world"))
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }

    @Test
    def parenExpressionShouldBeParsed():Unit = {
      val consumer = getConsumer("1 + (2 + 3);")

      val expression = ExpressionParser.parse(consumer)

      expression match {
        case Operation(_, _, exp2) =>
          exp2 match {
            case ParenExpression(expression) =>
              expression match {
                case Operation(exp1, operator, exp2) =>
                  exp1 match {
                    case LiteralNumber(value) => assert(value.component1() == 2)
                    case _ => assert(false)
                  }
                  assert(operator.component1().equals("+"))
                  exp2 match {
                    case LiteralNumber(value) => assert(value.component1() == 3)
                    case _ => assert(false)
                  }
                case _ => assert(false)
              }
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }
  }

  @Test
  def tokensShouldBeParsed():Unit = {
    val content = "let a:string = \"ab\"; a = \"hello world\";"
    val lexer = LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = new ParserImpl()

    val ast = parser.parse(content, tokens.asJava)

    ast match {
      case Root(sentences) =>
        sentences.head match {
          case DeclarationAssignation(declaration, assignation, expression) =>
            assert(declaration.declaration.component1().equals("let"))
            assert(declaration.declType.component1().equals("string"))
            assert(declaration.id.component1().equals("a"))
            assert(assignation.component1().equals("="))
            expression match {
              case LiteralString(value) => assert(value.component1().equals("ab"))
              case _ => assert(false)
            }
        }
        sentences(1) match {
          case VariableAssignation(variable, assignation, expression) =>
            assert(variable.value.component1().equals("a"))
            expression match {
              case LiteralString(value) => assert(value.component1().equals("hello world"))
              case _ => assert(false)
            }
          case _ => assert(false)
        }
      case _ => assert(false)
    }

    @Test
    def doubleParseExpressionShouldBeParsed(): Unit ={
      val consumer = getConsumer("((2))")

      val paren = ExpressionParser.parse(consumer)

      paren match {
        case ParenExpression(expression) =>
          expression match {
            case ParenExpression(expression) =>
              expression match {
                case LiteralNumber(value) => assert(value.component1() == 2)
              }
            case _ => assert(false)
          }
        case _ => assert(false)
      }
    }
  }

  @Test
  def contentWithoutStrategyShouldFail():Unit = {
    val content = "();"
    val lexer = LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = new ParserImpl()
    assertThrows(classOf[NoStrategyException], () => parser.parse(content, tokens.asJava))
  }

  @Test
  def declarationParserShouldExpectExpression():Unit = {
    val consumer = getConsumer("let a:string = let b:number")

    assertThrows(classOf[ExpressionExpectedException], () => DeclarationParser.parse(consumer))
  }

}
