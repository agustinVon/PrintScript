package parser

import ast.{ASTree, StringBranch}
import tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.Token
import org.austral.ingsis.printscript.parser.TokenIterator
import org.austral.ingsis.printscript.common.TokenConsumer
import parser.ParserStrategies.{Assignation, Declaration, Literal, PrintlnFunction, Variable, Operation}
import parser.exceptions.{NoContentInFileException, NoContentInLineException, NoStrategyException}
import parser.traits.{Parser, SectionParser}
import scala.annotation.tailrec

class ParserImpl extends Parser {
  private val strategies: List[SectionParser] = List(Declaration, Assignation, Literal, Variable, PrintlnFunction, Operation)
  override def parse(content:String, list: java.util.List[Token]): ASTree = {
    val tokenIterator = TokenIterator.create(content, list)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    buildTree(None, tokenConsumer)
  }

  def buildTree(tree: Option[ASTree], consumer: TokenConsumer):ASTree = {
    if (consumer.peek(TokenTypesImpl.EOF) != null) {
      tree.getOrElse(throw new NoContentInFileException)
    } else {
      tree match {
        case None => buildTree(Option(StringBranch(List(sentenceParse(None, consumer)), "Lines")), consumer)
        case Some(StringBranch(list, node)) => buildTree(Option(StringBranch(list :+ sentenceParse(None, consumer), node)), consumer)
        case _ => throw new IllegalArgumentException
      }
    }
  }

  @tailrec
  private final def sentenceParse(tree: Option[ASTree], consumer: TokenConsumer): ASTree = {
    if(consumer.peekAny(TokenTypesImpl.EOL, TokenTypesImpl.SEMICOLON) != null) {
      consumer.consumeAny(TokenTypesImpl.EOL, TokenTypesImpl.SEMICOLON)
      tree.getOrElse(throw NoContentInLineException())
    } else  {
      val strategy = strategies.find(strategy => strategy.canBeParsed(consumer)).getOrElse(throw NoStrategyException())
      val resultTree = strategy.parse(consumer, tree)
      sentenceParse(Option(resultTree), consumer)
    }
  }
}
