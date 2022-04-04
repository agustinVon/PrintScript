import ast.{Branch, Leaf, StringBranch}
import lexer.LexerImpl
import org.austral.ingsis.printscript.parser.TokenIterator
import org.junit.jupiter.api.Test
import parser.ParserStrategies.{Assignation, Declaration, Literal}
import parser.{ParserImpl, TokenConsumerImpl}
import tokens.TokenTypesImpl

import scala.jdk.CollectionConverters._

class ParserSuite  {
  @Test
  def declarationShouldBeaAbleToParse() = {
    val content= "let a:string"
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    Declaration.canBeParsed(tokenConsumer)
  }

  @Test
  def assignationShouldBeAbleToParse() = {
    val content= "="
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    Assignation.canBeParsed(tokenConsumer)
  }

  @Test
  def literalShouldBeAbleToParse() = {
    val content= "\"test\""
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    Literal.canBeParsed(tokenConsumer)
  }

  @Test
  def declarationShouldBeParsed(): Unit = {
    val content = "let a:string"
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))

    val ast = Declaration.parse(tokenConsumer, None)

    ast match {
      case Branch(list, node) =>
        assert(node.component1() == "let")
        assert(node.component2().getType == TokenTypesImpl.LET)
        list.head match {
          case Leaf(value) =>
            assert(value.component1() == "a")
            assert(value.component2().getType == TokenTypesImpl.IDENTIFIER)
          case _ => assert(false)
        }
        list(1) match {
          case Leaf(value) =>
            assert(value.component1() == "string")
            assert(value.component2().getType == TokenTypesImpl.TYPESTRING)
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  @Test
  def assignationShouldBeParsed(): Unit = {
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
              case Leaf(value) =>
                assert(value.component1() == "a")
                assert(value.component2().getType == TokenTypesImpl.IDENTIFIER)
              case _ => assert(false)
            }
            list(1) match {
              case Leaf(value) =>
                assert(value.component1() == "string")
                assert(value.component2().getType == TokenTypesImpl.TYPESTRING)
              case _ => assert(false)
            }
          case _ => assert(false)
        }
      case _ => assert(false)
    }
  }

  @Test
  def literalDeclarationShouldBeParsed() = {
    val content = "let a:string = \"test\""
    val tokens = new LexerImpl().lex(content).asJava
    val tokenConsumer = TokenConsumerImpl(TokenIterator.create(content, tokens))
    val declarationAst = Declaration.parse(tokenConsumer, None)
    val assignationAst = Assignation.parse(tokenConsumer, Option(declarationAst))

    val ast = Literal.parse(tokenConsumer, Option(assignationAst))

    ast match {
      case Branch(branches, node) =>
        assert(node.component1().equals("="))
        branches(1) match {
          case Leaf(value) =>
            value.component1().equals("\"test\"")
        }
    }
  }

  @Test
  def shouldParse2OrMoreSentences(): Unit = {
    val content = "let a:string = \"test\"; a + 2; println(a);"
    val tokens = new LexerImpl().lex(content).asJava
    val parser = new ParserImpl()

    val ast = parser.parse(content, tokens)

    ast match {
      case StringBranch(branches, node) =>
        assert(branches.length == 3)
        assert(node.equals("Lines"))
      case _ => assert(false)
    }
  }
}
