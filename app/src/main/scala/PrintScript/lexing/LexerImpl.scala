package PrintScript.lexing
import PrintScript.tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}

class LexerImpl extends Lexer{
  val matchers: Map[TokenType, LexerMatcher] = Matches().getMatches
  override def lex(source: String): List[Token] = {
    val matcher = LexerMatcherImpl.fromMatchers(matchers.values.toList).getMatcher(source)
    val tokens: List[Token] = List()
    var line = 0
    var position = 0
    var column = 0
    while (matcher.find()) {
      val lexMatch = matcher.group()
      lexMatch match {
        case "\n" => {
          line = line + 1
          column = 0
        }
        case _ => {
          val possibleTType = matchers.keys.find(key => matcher.group(key.getType) != null)
          possibleTType match {
            case None => {
              throw new RuntimeException
            }
            case Some(x) => {
              tokens :+ new Token(x, position, lexMatch.length, new LexicalRange(column, line, column + lexMatch.length, line ))
              column = lexMatch.length + column
              position = lexMatch.length + position
            }
          }
        }
        }
      }
    tokens :+ new Token(
      TokenTypesImpl.EOF,
      position,
      position,
      new LexicalRange(column, line, column, line)
    )
    tokens
  }
}
