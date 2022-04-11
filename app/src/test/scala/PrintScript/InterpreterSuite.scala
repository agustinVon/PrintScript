package PrintScript

import ast.{Declaration, DeclarationAssignation, Expression, LiteralNumber, LiteralString, Operation, ParenExpression, PrintLn, Root, Variable, VariableAssignation}
import lexer.{LexerImpl, StringProgramSource}
import org.austral.ingsis.printscript.parser.TokenIterator
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.{DisplayName, Nested, Test}
import parser.ParserStrategies.{DeclarationParser, ExpressionParser, FunctionParser, LiteralParser, VariableParser}
import parser.exceptions.{ExpressionExpectedException, NoStrategyException}
import parser.{ParserImpl, TokenConsumerImpl}

import scala.jdk.CollectionConverters._

class InterpreterSuite {

  @Test
  def declarationIntegerValueShouldBeStored(): Unit = {
    val content = "let x:number = 8;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = new ParserImpl().parse(content, tokens.asJava)
    val interpreter = new InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 8)
  }
}
