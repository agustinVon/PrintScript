package parser

import ast.{ASTree, Declaration, DeclarationAssignation, Expression, LiteralNumber, LiteralString, ParenExpression, PrintLn, Root, SumOrMinus, TimesOrDiv, Variable, VariableAssignation}
import org.austral.ingsis.printscript.common.{IntRead, Read, StringRead, TokenConsumer}
import parser.exceptions.ExpressionExpectedException
import parser.traits.{ExpressionSectionParser, OperationParser, SectionParser}
import tokens.TokenTypesImpl

object ParserStrategies {

  case object DeclarationParser extends SectionParser() {
    override def parse(consumer: TokenConsumer): ASTree = {
      val let        = consumer.consume(TokenTypesImpl.LET)
      val identifier = consumer.consume(TokenTypesImpl.IDENTIFIER)
      consumer.consume(TokenTypesImpl.COLON)
      val valType     = consumer.consumeAny(TokenTypesImpl.TYPESTRING, TokenTypesImpl.TYPENUMBER)
      val declaration = Declaration(let, identifier, valType)
      val shouldParseAssignment = consumer.peek(TokenTypesImpl.ASSIGNMENT) != null
      if (shouldParseAssignment) {
        val assignment = consumer.consume(TokenTypesImpl.ASSIGNMENT)
        if (ExpressionParser.canBeParsed(consumer)) {
          val expression = ExpressionParser.parse(consumer)
          DeclarationAssignation(declaration, assignment, expression)
        } else {
          throw ExpressionExpectedException(
            consumer.current().getRange.getStartLine,
            consumer.current().getRange.getStartCol
          )
        }
      } else {
        declaration
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = {
      consumer.peek(TokenTypesImpl.LET) != null
    }
  }

  case object ExpressionParser extends ExpressionSectionParser {
    override def parse(consumer: TokenConsumer): Expression = {
      val exp = UnitParser.parse(consumer)
      if (SumOrMinusParser.canBeParsed(consumer)) {
        SumOrMinusParser.parse(exp, consumer)
      } else {
        exp
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = UnitParser.canBeParsed(consumer)
  }

  case object SumOrMinusParser extends OperationParser {
    override def parse(exp: Expression, consumer: TokenConsumer): Expression = {
      if (consumer.peekAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS) != null) {
        val operator = consumer.consumeAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS)
        val lit = UnitParser.parse(consumer)
        if (TimesOrDivParser.canBeParsed(consumer)) {
          val op = TimesOrDivParser.parse(lit, consumer)
          val sum = SumOrMinus(exp, operator, op)
          if (canBeParsed(consumer)) {
            parse(sum, consumer)
          } else {
            sum
          }
        } else if (canBeParsed(consumer)) {
          val op = parse(lit, consumer)
          SumOrMinus(exp, operator, op)
        } else {
          SumOrMinus(exp, operator, lit)
        }
      } else if (TimesOrDivParser.canBeParsed(consumer)) {
        val op = TimesOrDivParser.parse(exp, consumer)
        parse(op, consumer)
      } else {
        exp
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peekAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS, TokenTypesImpl.TIMES, TokenTypesImpl.DIVIDEDBY) != null
  }

  case object TimesOrDivParser extends OperationParser {
    override def parse(exp: Expression, consumer: TokenConsumer): Expression = {
      val operator = consumer.consumeAny(TokenTypesImpl.TIMES, TokenTypesImpl.DIVIDEDBY)
      val lit = UnitParser.parse(consumer)
      if (TimesOrDivParser.canBeParsed(consumer)) {
        val op = TimesOrDivParser.parse(lit, consumer)
        TimesOrDiv(exp, operator, op)
      } else {
        TimesOrDiv(exp, operator, lit)
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peekAny(TokenTypesImpl.TIMES, TokenTypesImpl.DIVIDEDBY) != null
  }

  case object UnitParser extends ExpressionSectionParser() {
    override def parse(consumer: TokenConsumer): Expression = {
      if (LiteralParser.canBeParsed(consumer)) {
        LiteralParser.parse(consumer)
      } else if (VariableParser.canBeParsed(consumer)){
        val variable = VariableParser.parse(consumer)
        variable match {
          case expression: Expression =>
            expression
          case _ =>
            throw ExpressionExpectedException(
              consumer.current().component4().getStartLine,
              consumer.current().component4().getStartCol
            )
        }
      } else if (ParenParser.canBeParsed(consumer)){
        ParenParser.parse(consumer)
      } else {
        throw ExpressionExpectedException(
          consumer.current().component4().getStartLine,
          consumer.current().component4().getStartCol
        )
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = LiteralParser.canBeParsed(consumer) || VariableParser.canBeParsed(consumer) || ParenParser.canBeParsed(consumer)
  }

  case object VariableParser extends SectionParser() {
    override def parse(consumer: TokenConsumer): ASTree = {
      val variable = Variable(consumer.consume(TokenTypesImpl.IDENTIFIER))
      if (consumer.peek(TokenTypesImpl.ASSIGNMENT) != null) {
        val assignment = consumer.consume(TokenTypesImpl.ASSIGNMENT)
        val expression = ExpressionParser.parse(consumer)
        VariableAssignation(variable, assignment, expression)
      } else {
        variable
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.IDENTIFIER) != null
  }

  case object LiteralParser extends ExpressionSectionParser() {
    object MyIntRead extends Read[Double] {
      override def read(content: String, from: Int, to: Int): Double = {
        return StringRead.INSTANCE.read(content, from, to).toDouble
      }
      override def default(): Double = {
        0
      }
    }

    override def parse(consumer: TokenConsumer): Expression = {
      if (consumer.peek(TokenTypesImpl.STRING) != null) {
        LiteralString(consumer.consume(TokenTypesImpl.STRING))
      } else {
        LiteralNumber(consumer.consume(TokenTypesImpl.NUMBER, MyIntRead))

      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean =
      consumer.peekAny(TokenTypesImpl.STRING, TokenTypesImpl.NUMBER) != null
  }

  case object ParenParser extends ExpressionSectionParser() {
    override def parse(consumer: TokenConsumer): Expression = {
      consumer.consume(TokenTypesImpl.OPENPAREN)
      if (ExpressionParser.canBeParsed(consumer)) {
        val exp = ParenExpression(ExpressionParser.parse(consumer))
        consumer.consume(TokenTypesImpl.CLOSEPAREN)
        exp
      } else {
        throw ExpressionExpectedException(
          consumer.current().component4().getStartLine,
          consumer.current().component4().getStartCol
        )
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.OPENPAREN) != null
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
        throw ExpressionExpectedException(
          consumer.current().component4().getStartLine,
          consumer.current().component4().getStartCol
        )
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.PRINTLN) != null
  }
}
