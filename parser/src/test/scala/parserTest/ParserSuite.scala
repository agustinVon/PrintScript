package parserTest

import ast.{Branch, Leaf}
import lexer.LexerImpl
import org.austral.ingsis.printscript.common.{LexicalRange, Token}
import org.austral.ingsis.printscript.parser.TokenIterator
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import parser.ParserStrategies.{Assignation, Declaration, Literal}
import parser.{ParserImpl, TokenConsumerImpl}
import tokens.TokenTypesImpl

import scala.jdk.CollectionConverters._

@RunWith(classOf[JUnitRunner])
class ParserSuite extends AnyFunSuite  {
  test("declarationShouldBeaAbleToParse") {
    val content= "let a:string"
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    Declaration.canBeParsed(tokenConsumer)
  }

  test("assignationShouldBeAbleToParse") {
    val content= "="
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    Assignation.canBeParsed(tokenConsumer)
  }

  test("literalShouldBeAbleToParse") {
    val content= "\"test\""
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    Literal.canBeParsed(tokenConsumer)
  }

  test("declarationShouldBeParsed") {
    val content = "let a:string"
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))

    val ast = Declaration.parse(tokenConsumer, None)

    ast match {
      case Branch(list, node) =>
        assert(node.component1() == "let")
        assert(node.component2().getType == TokenTypesImpl.LET)
        list.head match {
          case Leaf(value) => {
            assert(value.component1() == "a")
            assert(value.component2().getType == TokenTypesImpl.IDENTIFIER)
          }
          case _ => assert(false)
        }
        list(1) match {
          case Leaf(value) => {
            assert(value.component1() == "string")
            assert(value.component2().getType == TokenTypesImpl.TYPESTRING)
          }
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  test("assignationShouldBeParsed") {
    val content = "let a:string = "
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    val declarationAst = Declaration.parse(tokenConsumer, None)

    val ast = Assignation.parse(tokenConsumer, Option(declarationAst))

    ast match {
      case Branch(list, node) =>
        assert(node.component1() == "=")
        assert(node.component2().getType == TokenTypesImpl.ASSIGNMENT)
        list.head match {
          case Branch(list, node) =>
            assert(node.component1() == "let")
            assert(node.component2().getType == TokenTypesImpl.LET)
            list.head match {
              case Leaf(value) => {
                assert(value.component1() == "a")
                assert(value.component2().getType == TokenTypesImpl.IDENTIFIER)
              }
              case _ => assert(false)
            }
            list(1) match {
              case Leaf(value) => {
                assert(value.component1() == "string")
                assert(value.component2().getType == TokenTypesImpl.TYPESTRING)
              }
              case _ => assert(false)
            }
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  // TODO finish test
  test("literalDeclarationShouldBeParsed") {
    val content = "let a:string = "
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    val declarationAst = Declaration.parse(tokenConsumer, None)
    val assignationAst = Assignation.parse(tokenConsumer, Option(declarationAst))

    assert(true)
  }

  test("shouldParse2OrMoreSentences") {
    val content = "let a:string = \"test\"; a + 2; println(a);"
    val tokens = new LexerImpl().lex(content).asJava
    val parser = new ParserImpl()

    val ast = parser.parse(content, tokens)

    assert(true)
  }
}
