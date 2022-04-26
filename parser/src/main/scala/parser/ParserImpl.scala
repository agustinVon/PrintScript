package parser

import ast.{ASTree, Root}
import tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.{Token, TokenConsumeException, TokenConsumer}
import org.austral.ingsis.printscript.parser.TokenIterator
import parser.ParserStrategies.{
  DeclarationParser,
  IfParser,
  LiteralParser,
  PrintLnParser,
  ReadInputParser,
  VariableParser
}
import parser.exceptions.{ExpectedEndOfLineException, NoStrategyException}
import parser.traits.{Parser, SectionParser}
import sources.ProgramSource
import org.austral.ingsis.printscript.common.TokenType

import scala.jdk.CollectionConverters._

case class ParserImpl() extends Parser {
  private val strategies: List[SectionParser] =
    List(DeclarationParser, LiteralParser, VariableParser, PrintLnParser, IfParser, ReadInputParser)
  override def parse(content: ProgramSource, list: List[Token]): ASTree = {
    val tokenIterator = TokenIterator.create(content.getSourceString, list.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    buildTree(Root(List()), tokenConsumer, TokenTypesImpl.EOF)
  }

  def buildTree(tree: Root, consumer: TokenConsumer, untilToken: TokenType): Root = {
    if (consumer.peek(untilToken) != null) {
      tree
    } else {
      buildTree(Root(tree.sentences :+ sentenceParse(consumer)), consumer, untilToken)
    }
  }

  private final def sentenceParse(consumer: TokenConsumer): ASTree = {
    val strategy   = strategies.find(strategy => strategy.canBeParsed(consumer)).getOrElse(throw NoStrategyException())
    val resultTree = strategy.parse(consumer)
    try {
      consumer.consumeAny(TokenTypesImpl.EOL, TokenTypesImpl.SEMICOLON)
    } catch {
      case e: TokenConsumeException =>
        throw ExpectedEndOfLineException(
          e.getMessage,
          consumer.current().component4().getStartLine,
          consumer.current().component4().getStartCol
        )
    }
    resultTree
  }
}
