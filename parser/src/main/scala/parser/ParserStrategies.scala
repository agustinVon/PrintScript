package parser

import ast.{ASTree, Declaration, DeclarationAssignation, Expression, LiteralNumber, LiteralString, Operation, PrintLn, Root, Variable, VariableAssignation}
import org.austral.ingsis.printscript.common.{IntRead, Read, StringRead, TokenConsumer}
import parser.exceptions.ExpressionExpectedException
import parser.traits.{ExpressionSectionParser, SectionParser}
import tokens.TokenTypesImpl

object ParserStrategies {

  case object DeclarationParser extends SectionParser() {
    override def parse(consumer: TokenConsumer): ASTree = {
      val let = consumer.consume(TokenTypesImpl.LET)
      val identifier = consumer.consume(TokenTypesImpl.IDENTIFIER)
      consumer.consume(TokenTypesImpl.COLON)
      val valType = consumer.consumeAny(TokenTypesImpl.TYPESTRING, TokenTypesImpl.TYPENUMBER)
      val declaration = Declaration(let, identifier, valType)
      if (consumer.peek(TokenTypesImpl.ASSIGNMENT) != null) {
        val assignment = consumer.consume(TokenTypesImpl.ASSIGNMENT)
        if (ExpressionParser.canBeParsed(consumer)) {
          val expression = ExpressionParser.parse(consumer)
          DeclarationAssignation(declaration, assignment, expression)
        } else {
          throw ExpressionExpectedException()
        }
      } else {
        declaration
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = {
      consumer.peek(TokenTypesImpl.LET) != null
    }
  }

  private def parseOperation(consumer: TokenConsumer, expression: Expression): Expression = {
    if (consumer.peekAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS, TokenTypesImpl.DIVIDEDBY, TokenTypesImpl.TIMES) != null) {
      val operator = consumer.consumeAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS, TokenTypesImpl.DIVIDEDBY, TokenTypesImpl.TIMES)
      if (ExpressionParser.canBeParsed(consumer)) {
        Operation(expression, operator, ExpressionParser.parse(consumer))
      } else {
        throw ExpressionExpectedException()
      }
    } else {
      expression
    }
  }

  case object ExpressionParser extends ExpressionSectionParser {
    override def parse(consumer: TokenConsumer): Expression = {
      if (LiteralParser.canBeParsed(consumer)) {
        val literal = LiteralParser.parse(consumer)
        parseOperation(consumer, literal)
      } else {
        val variable = VariableParser.parse(consumer)
        variable match {
          case expression: Expression =>
            parseOperation(consumer, expression)
          case _ => throw ExpressionExpectedException()
        }
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = {
      consumer.peekAny(TokenTypesImpl.STRING, TokenTypesImpl.NUMBER, TokenTypesImpl.IDENTIFIER) != null
    }
  }

  case object LiteralParser extends ExpressionSectionParser() {
    object MyIntRead extends Read[Int] {
      override def default(): Int = IntRead.INSTANCE.default()

      override def read(s: String, i: Int, i1: Int): Int = IntRead.INSTANCE.read(s, i, i1)
    }

    override def parse(consumer: TokenConsumer): Expression = {
      if (consumer.peek(TokenTypesImpl.STRING) != null) {
        LiteralString(consumer.consume(TokenTypesImpl.STRING))
      } else {
        LiteralNumber(consumer.consume(TokenTypesImpl.NUMBER, MyIntRead))
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peekAny(TokenTypesImpl.STRING, TokenTypesImpl.NUMBER) != null
  }

  case object VariableParser extends SectionParser() {
    override def parse(consumer: TokenConsumer): ASTree = {
      val variable = Variable(consumer.consume(TokenTypesImpl.IDENTIFIER))
      if (consumer.peek(TokenTypesImpl.ASSIGNMENT)!= null) {
        val assignment = consumer.consume(TokenTypesImpl.ASSIGNMENT)
        val expression = ExpressionParser.parse(consumer)
        VariableAssignation(variable, assignment, expression)
      } else {
        variable
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.IDENTIFIER) != null
  }

  case object FunctionParser extends SectionParser() {
    override def parse(consumer: TokenConsumer): ASTree = {
      val function = consumer.consume(TokenTypesImpl.PRINTLN)
      consumer.consume(TokenTypesImpl.OPENPAREN)
      if (ExpressionParser.canBeParsed(consumer)) {
        val expression = ExpressionParser.parse(consumer)
        consumer.consume(TokenTypesImpl.CLOSEPAREN)
        PrintLn(function, expression)
      } else {
        throw ExpressionExpectedException()
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.PRINTLN) != null
  }
}
