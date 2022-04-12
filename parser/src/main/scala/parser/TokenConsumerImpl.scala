package parser

import org.austral.ingsis.printscript.common.TokenConsumer
import org.austral.ingsis.printscript.parser.TokenIterator

case class TokenConsumerImpl(tokenIterator: TokenIterator) extends TokenConsumer(tokenIterator)
