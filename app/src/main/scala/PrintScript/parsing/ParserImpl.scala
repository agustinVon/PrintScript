package PrintScript.parsing
import PrintScript.tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.{StringRead, Token, TokenConsumer}
import org.austral.ingsis.printscript.parser.TokenIterator

import scala.jdk.CollectionConverters._

class ParserImpl extends Parser {
  var tree:Option[ASTree] = None : Option[ASTree]
  override def parse(content:String, list: List[Token]): ASTree = {
    val tokenIterator = TokenIterator.create(content, list.asJava)
    val tokenConsumer = TokenConsumerImpl(tokenIterator)
    val firstToken = tokenConsumer.peekAny(TokenTypesImpl.LET)
    firstToken.getToken.getType match {
      case TokenTypesImpl.LET =>
        tokenConsumer.consume(TokenTypesImpl.LET)
        tokenConsumer.consume(TokenTypesImpl.IDENTIFIER)
        tokenConsumer.consume(TokenTypesImpl.COLON)
        val typePeek = tokenConsumer.peekAny(TokenTypesImpl.TYPESTRING, TokenTypesImpl.TYPENUMBER)
        typePeek.getToken.getType match {
          case TokenTypesImpl.TYPESTRING =>
            tokenConsumer.consume(TokenTypesImpl.TYPESTRING)
            tokenConsumer.consume(TokenTypesImpl.ASSIGNMENT)
            tokenConsumer.consume(TokenTypesImpl.STRING)
          case _ =>
            tokenConsumer.consume(TokenTypesImpl.TYPENUMBER)
            tokenConsumer.consume(TokenTypesImpl.ASSIGNMENT)
            tokenConsumer.consume(TokenTypesImpl.NUMBER)
        }
    }
    tree.getOrElse(throw new RuntimeException)
      tree.getOrElse(throw new RuntimeException)
  }
}
