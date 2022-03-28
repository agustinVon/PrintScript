package parser

import ast.{ASTree, Branch, Leaf, StringBranch}
import org.austral.ingsis.printscript.common.TokenConsumer
import parser.traits.{Parser, SectionParser}
import tokens.TokenTypesImpl

object ParserStrategies {

  case object Declaration extends SectionParser() {
    override def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree = {
      val declaration = consumer.consume(TokenTypesImpl.LET)
      val identifier = consumer.consume(TokenTypesImpl.IDENTIFIER)
      consumer.consume(TokenTypesImpl.COLON)
      val valType = consumer.consumeAny(TokenTypesImpl.TYPESTRING, TokenTypesImpl.TYPENUMBER)
      val result = Branch(List(Leaf(identifier), Leaf(valType)), declaration)
      tree match {
        case None => result
        case _ => throw new IllegalArgumentException()
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.LET) != null
  }

  case object Assignation extends SectionParser() {
    override def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree = {
      val assignation = consumer.consume(TokenTypesImpl.ASSIGNMENT)
      tree match {
        case Some(Leaf(x)) => Branch(List(Leaf(x)), assignation)
        case Some(Branch(list, node)) => Branch(List(Branch(list, node)), assignation)
        case _ => throw new IllegalArgumentException()
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.ASSIGNMENT) != null
  }

  case object Literal extends SectionParser() {
    override def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree = {
      val value = consumer.consumeAny(TokenTypesImpl.NUMBER, TokenTypesImpl.STRING)
      val result = Leaf(value)
      tree match {
        case None => result
        case Some(Branch(list, node)) => Branch(list :+ result, node)
        case _ => throw new IllegalArgumentException()
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peekAny(TokenTypesImpl.STRING, TokenTypesImpl.NUMBER) != null
  }

  case object Operation extends SectionParser() {
    override def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree = {
      val operation = consumer.consumeAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS, TokenTypesImpl.DIVIDEDBY, TokenTypesImpl.TIMES)
      tree match {
        case Some(Leaf(x)) => Branch(List(Leaf(x)), operation)
        case Some(Branch(list, node)) => Branch(List(Branch(list, node)), operation)
        case _ => throw new IllegalArgumentException
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peekAny(TokenTypesImpl.PLUS, TokenTypesImpl.MINUS, TokenTypesImpl.DIVIDEDBY, TokenTypesImpl.TIMES) != null
  }

  case object Variable extends SectionParser() {
    override def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree = {
      val identifier = consumer.consume(TokenTypesImpl.IDENTIFIER)
      val result = Leaf(identifier)
      tree match {
        case None => result
        case Some(Branch(list, node)) => Branch(list :+ result, node)
        case _ => throw new IllegalArgumentException()
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.IDENTIFIER) != null
  }

  case object PrintlnFunction extends SectionParser() {
    override def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree = {
      val function = consumer.consume(TokenTypesImpl.PRINTLN)
      consumer.consume(TokenTypesImpl.OPENPAREN)
      val string = consumer.consumeAny(TokenTypesImpl.STRING, TokenTypesImpl.IDENTIFIER)
      consumer.consume(TokenTypesImpl.CLOSEPAREN)
      val result = Branch(List(Leaf(string)), function)
      tree match {
        case None => result
        case _ => throw new IllegalArgumentException()
      }
    }

    override def canBeParsed(consumer: TokenConsumer): Boolean = consumer.peek(TokenTypesImpl.PRINTLN) != null
  }
}
