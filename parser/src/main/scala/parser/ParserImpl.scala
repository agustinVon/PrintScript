package parser

import ast.{ASTree, Root}
import tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.{Token, TokenConsumeException, TokenConsumer}
import org.austral.ingsis.printscript.parser.TokenIterator
import parser.ParserStrategies.{DeclarationParser, FunctionParser, LiteralParser, VariableParser}
import parser.exceptions.{ExpectedEndOfLineException, NoStrategyException}
import parser.traits.{Parser, SectionParser}
import sources.ProgramSource

import scala.jdk.CollectionConverters._

case class ParserImpl() extends Parser {
  private val strategies: List[SectionParser] = List(DeclarationParser, LiteralParser, VariableParser, FunctionParser)
  override def parse(content:ProgramSource, list: List[Token]): ASTree = {
    val tokenIterator = TokenIterator.create(content.getSourceString, list.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    buildTree(Root(List()), tokenConsumer)
  }

  def buildTree(tree: Root, consumer: TokenConsumer):ASTree = {
    if (consumer.peek(TokenTypesImpl.EOF) != null) {
      tree
    } else {
      buildTree(Root(tree.sentences :+ sentenceParse(consumer)), consumer)
    }
  }

  private final def sentenceParse(consumer: TokenConsumer): ASTree = {
    val strategy = strategies.find(strategy => strategy.canBeParsed(consumer)).getOrElse(throw NoStrategyException())
    val resultTree = strategy.parse(consumer)
    try {
      consumer.consumeAny(TokenTypesImpl.EOL, TokenTypesImpl.SEMICOLON)
    } catch {
      case e:TokenConsumeException => throw ExpectedEndOfLineException(e.getMessage, consumer.current().component4().getStartLine, consumer.current().component4().getStartCol)
    }
    resultTree
  }
}
