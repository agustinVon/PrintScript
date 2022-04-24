import ast.{Declaration, DeclarationAssignation, Expression, IfCodeBlock, IfElseCodeBlock, LiteralBoolean, LiteralNumber, LiteralString, ParenExpression, PrintLn, ReadInput, Root, SumOrMinus, TimesOrDiv, Variable, VariableAssignation}
import lexer.LexerImpl
import org.austral.ingsis.printscript.parser.TokenIterator
import org.junit.jupiter.api.Assertions.{assertFalse, assertThrows}
import org.junit.jupiter.api.{DisplayName, Nested, Test}
import parser.ParserStrategies.{DeclarationParser, ExpressionParser, IfParser, LiteralParser, PrintLnParser, ReadInputParser, VariableParser}
import parser.exceptions.{ExpressionExpectedException, NoStrategyException, NotABooleanExpressionException}
import parser.{ParserImpl, TokenConsumerImpl}
import sources.StringProgramSource

import scala.jdk.CollectionConverters._

class ParserSuite  {
  private def getConsumer(content:String): TokenConsumerImpl = {
    val source = StringProgramSource(content)
    val tokens = LexerImpl().lex(source).asJava
    TokenConsumerImpl(TokenIterator.create(content, tokens))
  }

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
  def literalDoubleShouldBeAbleToParse():Unit = {
    val consumer = getConsumer("2.5")

    assert(LiteralParser.canBeParsed(consumer))
  }

  @Test
  def functionParserShouldBeAbleToParse():Unit = {
    val consumer = getConsumer("println(\"test\");")

    assert(PrintLnParser.canBeParsed(consumer))
  }

  @Test
  def parenExpressionShouldBeAbleToParse():Unit = {
    val consumer = getConsumer("(5 + 2)")

    assert(ExpressionParser.canBeParsed(consumer))
  }

  @Test
  def multipleParenExpressionShouldBeParsed():Unit = {
    val consumer = getConsumer("((5 + 2) * 4)")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case ParenExpression(expression) =>
        expression match {
          case TimesOrDiv(exp1, operator, exp2) =>
            assert(operator.component1().equals("*"))
            exp1 match {
              case ParenExpression(expression) => {
                expression match {
                  case SumOrMinus(exp1, operator, exp2) =>
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

  @Test
  def expressionParserShouldParseLiteralDoubles():Unit = {
    val consumer = getConsumer("2.5")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case LiteralNumber(value) => assert(value.component1().equals(2.5))
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

    val function = PrintLnParser.parse(consumer)

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
      case SumOrMinus(_, _, exp2) =>
        exp2 match {
          case ParenExpression(expression) =>
            expression match {
              case SumOrMinus(exp1, operator, exp2) =>
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

  @Test
  def tokensShouldBeParsed():Unit = {
    val content = "let a:string = \"ab\"; a = \"hello world\";"
    val lexer = LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = new ParserImpl()

    val ast = parser.parse(StringProgramSource(content), tokens)

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

  @Test
  def expressionParserShouldParseOperations(): Unit = {
    val consumer = getConsumer("2 + 4")

    val expression = ExpressionParser.parse(consumer)

    expression match {
      case SumOrMinus(exp1, operator, exp2) =>
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
      case SumOrMinus(exp1, operator, exp2) =>
        exp1 match {
          case Variable(value) => assert(value.component1().equals("a"))
          case _ => assert(false)
        }
        assert(operator.component1().equals("+"))
        exp2 match {
          case SumOrMinus(exp3, operator, exp4) =>
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
  def assignationOfSumShouldAssign():Unit = {
    val consumer = getConsumer("a = 4 + 5")

    val assignation = VariableParser.parse(consumer)

    assignation match {
      case VariableAssignation(variable, assignation, expression) =>
        assert(variable.value.component1().equals("a"))
        assert(assignation.component1().equals("="))
        expression match {
          case SumOrMinus(exp1, operator, exp2) =>
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
  def contentWithoutStrategyShouldFail():Unit = {
    val content = "();"
    val lexer = LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = new ParserImpl()
    assertThrows(classOf[NoStrategyException], () => parser.parse(StringProgramSource(content), tokens))
  }

  @Test
  def declarationParserShouldExpectExpression():Unit = {
    val consumer = getConsumer("let a:string = let b:number")

    assertThrows(classOf[ExpressionExpectedException], () => DeclarationParser.parse(consumer))
  }

  @Test
  def operationParserShouldParse():Unit = {
    val consumer = getConsumer("a + b + 1")

    val sum = ExpressionParser.parse(consumer)

    sum match {
      case SumOrMinus(exp1, operator, exp2) =>
        exp1 match {
          case Variable(value) => value.component1().equals("a")
          case _ => assert(false)
        }
        operator.component1().equals("+")
        exp2 match {
          case SumOrMinus(exp1, operator, exp2) =>
            exp1 match {
              case Variable(value) => value.component1().equals("b")
              case _ => assert(false)
            }
            operator.component1().equals("+")
            exp2 match {
              case LiteralNumber(value) => value.component1() == 1
              case _ => assert(false)
            }
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  @Test
  def timesDivParserShouldParse(): Unit = {
    val consumer = getConsumer("2 * 3")
    val times = ExpressionParser.parse(consumer)
    times match {
      case TimesOrDiv(exp1, operator, exp2) =>
        exp1 match {
          case LiteralNumber(value) => value.component1() == 2
          case _ => assert(false)
        }
        operator.component1().equals("*")
        exp2 match {
          case LiteralNumber(value) => value.component1() == 3
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  @Test
  def sumWithTimesShouldBeParsed(): Unit = {
    val consumer = getConsumer("2 * 3 + 8 * 4 + 9")
    val sum = ExpressionParser.parse(consumer)

    sum match {
      case SumOrMinus(exp1, operator, exp2) =>
        exp1 match {
          case SumOrMinus(exp1, operator, exp2) =>
            exp1 match {
              case TimesOrDiv(exp1, operator, exp2) =>
                exp1 match {
                  case LiteralNumber(value) => value.component1() == 2
                  case _ => assert(false)
                }
                exp2 match {
                  case LiteralNumber(value) => value.component1() == 3
                  case _ => assert(false)
                }
              case _ => assert(false)
            }
            exp2 match {
              case TimesOrDiv(exp1, operator, exp2) =>
                exp1 match {
                  case LiteralNumber(value) => value.component1() == 8
                  case _ => assert(false)
                }
                exp2 match {
                  case LiteralNumber(value) => value.component1() == 4
                  case _ => assert(false)
                }
              case _ => assert(false)
            }
          case _ => assert(false)
        }
        exp2 match {
          case LiteralNumber(value) => value.component1() == 9
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  @Test
  def doubleSimbolOperationShouldNotBeParsed():Unit = {
    val consumer = getConsumer("2 ++ 3")
    assertThrows(classOf[ExpressionExpectedException], () => ExpressionParser.parse(consumer))
  }

  @Test
  def ifBlockShouldBeAbleToParse():Unit = {
    val consumer = getConsumer("if")

    assert(IfParser.canBeParsed(consumer))
  }

  @Test
  def ifBlockShouldBeParsed():Unit = {
    val consumer = getConsumer("if(true){ a = 4; b = a + 5; }")

    val ifBlock = IfParser.parse(consumer)

    ifBlock match {
      case IfCodeBlock(condition, codeBlock) => {
        condition match {
          case LiteralBoolean(value) => assert(value.component1())
          case _ => assert(false)
        }
        assert(codeBlock.sentences.length == 2)
      }
    }
  }

  @Test
  def declarationAssignationOfBooleanShouldBeParsed():Unit = {
    val consumer = getConsumer("let a:boolean = false")

    val declaration = DeclarationParser.parse(consumer)

    declaration match {
      case DeclarationAssignation(declaration, assignation, expression) =>
        assert(declaration.declType.component1().equals("boolean"))
        expression match {
          case LiteralBoolean(value) => assertFalse(value.component1())
        }
    }
  }

  @Test
  def parserShouldBeAbleToParseIfStatements():Unit = {
    val content = "if (true) { let a:number = 10 * 5 + 8; println(a); };"
    val lexer = LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = ParserImpl()

    val ast = parser.parse(StringProgramSource(content), tokens)

    ast match {
      case Root(sentences) =>
        sentences.head match {
          case IfCodeBlock(condition, block) =>
            condition match {
              case LiteralBoolean(value) => assert(value.component1())
              case _ => assert(false)
            }
            assert(block.sentences.length == 2)
          case _ => assert(false)
        }
    }
  }

  @Test
  def ifElseBlockShouldBeParsed():Unit = {
    val consumer = getConsumer("if(true){ a = 4; b = a + 5; } else { a = 7; }")

    val ifElseBlock = IfParser.parse(consumer)

    ifElseBlock match {
      case IfElseCodeBlock(condition, ifCodeBlock, elseCodeBlock) => {
        condition match {
          case LiteralBoolean(value) => assert(value.component1())
          case _ => assert(false)
        }
        assert(ifCodeBlock.sentences.length == 2)
        assert(elseCodeBlock.sentences.length == 1)
      }
    }
  }

  @Test
  def parserShouldBeAbleToParseIfElseStatements():Unit = {
    val content = "if (true) { let a:number = 10 * 5 + 8; println(a); } else { b = c * 9; };"
    val lexer = LexerImpl()
    val tokens = lexer.lex(StringProgramSource(content))
    val parser = ParserImpl()

    val ast = parser.parse(StringProgramSource(content), tokens)

    ast match {
      case Root(sentences) =>
        sentences.head match {
          case IfElseCodeBlock(condition, ifBlock, elseBlock) =>
            condition match {
              case LiteralBoolean(value) => assert(value.component1())
              case _ => assert(false)
            }
            assert(ifBlock.sentences.length == 2)
            assert(elseBlock.sentences.length == 1)
          case _ => assert(false)
        }
    }
  }

  @Test
  def ifConditionIsNotBooleanShouldFail():Unit = {
    val consumer = getConsumer("if(4) { }")

    assertThrows(classOf[NotABooleanExpressionException], () => IfParser.parse(consumer))
  }

  @Test
  def readInputShouldBeAbleToParse():Unit = {
    val consumer = getConsumer("readInput(\"Input your value\")")

    val function = ReadInputParser.parse(consumer)

    function match {
      case ReadInput(function, message) =>
        assert(function.component1().equals("readInput"))
        message match {
          case LiteralString(value) => assert(value.component1().equals("Input your value"))
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  @Test
  def constDeclarationShouldBeAbleToParse():Unit = {
    val consumer = getConsumer("const a:string")

    assert(DeclarationParser.canBeParsed(consumer))
  }

  @Test
  def constDeclarationShouldBeParsed():Unit = {
    val consumer = getConsumer("const a:string")

    val declaration = DeclarationParser.parse(consumer)

    declaration match {
      case Declaration(declaration, _, _) =>
        assert(declaration.component1().equals("const"))
      case _ => assert(false)
    }
  }

}
