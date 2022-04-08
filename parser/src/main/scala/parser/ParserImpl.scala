package parser

import ast.{ASTree, Root}
import tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.Token
import org.austral.ingsis.printscript.parser.TokenIterator
import org.austral.ingsis.printscript.common.TokenConsumer
import parser.ParserStrategies.{DeclarationParser, FunctionParser, LiteralParser, VariableParser}
import parser.exceptions.NoStrategyException
import parser.traits.{Parser, SectionParser}

import scala.annotation.tailrec

class ParserImpl extends Parser {
  private val strategies: List[SectionParser] = List(DeclarationParser, LiteralParser, VariableParser, FunctionParser)
  override def parse(content:String, list: java.util.List[Token]): ASTree = {
    val tokenIterator = TokenIterator.create(content, list)
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
    consumer.consumeAny(TokenTypesImpl.EOL, TokenTypesImpl.SEMICOLON)
    resultTree
  }
}
