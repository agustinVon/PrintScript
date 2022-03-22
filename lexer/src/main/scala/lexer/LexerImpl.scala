package lexer

import Tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}

class LexerImpl extends Lexer{
  val matchers: List[(TokenType, LexerMatcher)] = Matches().getMatches
  override def lex(source: String): List[Token] = {
    val matcher = LexerMatcherImpl.fromMatchers(matchers.map(_._2)).getMatcher(source)
    var tokens: List[Token] = List()
    var line = 0
    var position = 0
    var column = 0
    while (matcher.find()) {
      val lexMatch = matcher.group()
      val possibleTType = matchers.map(_._1).find(key => matcher.group(key.getType) != null)
      possibleTType match {
        case None => {
          throw new RuntimeException
        }
        case Some(TokenTypesImpl.WHITESPACE) => {
          column = lexMatch.length + column
          position = lexMatch.length + position
        }
        case Some(TokenTypesImpl.EOL) => {
          column = 0
          position = lexMatch.length + position
          line = line + 1
        }
        case Some(x) => {
          tokens = tokens :+ new Token(x, position, position + lexMatch.length, new LexicalRange(column, line, column + lexMatch.length, line ))
          column = lexMatch.length + column
          position = lexMatch.length + position
        }
      }
      }
    tokens :+ new Token(
      TokenTypesImpl.EOF,
      position,
      position,
      new LexicalRange(column, line, column, line)
    )
  }
}
