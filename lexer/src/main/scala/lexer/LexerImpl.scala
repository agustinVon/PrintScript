package lexer

import tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.{LexicalRange, Token, TokenType}

import java.util.regex.{MatchResult, Matcher}

case class LexerImpl() extends Lexer {

  type MatchResult = (String, Option[TokenType])
  type TokenGenerationResult = (Option[Token], Int, Int, Int)
  val matchers: List[(TokenType, LexerMatcher)] = Matches().getMatches

  override def lex(source: ProgramSource): List[Token] = {
    val matcher = getMainMatcher(source.getSourceString)
    getTokensListFromMatcher(matcher)
  }

  private def getMainMatcher(source:String):Matcher = {
    LexerMatcherImpl.fromMatchers(matchers.map(_._2)).getMatcher(source)
  }

  private def getTokensListFromMatcher(matcher: Matcher):List[Token] = {
    var tokens: List[Token] = List()
    var line, position, column = 0
    while (matcher.find()) {
      val matchResult:MatchResult = getMatchResult(matcher)
      val (token, l, p, c) = createTokenFromMatchResult(matchResult:MatchResult, line:Int, position:Int, column:Int)
      token match {
        case Some(x) => tokens = tokens:+ x
        case None =>
      }
      line = l
      position = p
      column = c
    }
    tokens :+ new Token(TokenTypesImpl.EOF, position, position, new LexicalRange(column, line, column, line))
  }

  private def getMatchResult(matcher:Matcher):MatchResult = {
    (matcher.group(), matchers.map(_._1).find(key => matcher.group(key.getType) != null))
  }

  private def createTokenFromMatchResult(matchResult: MatchResult, line: Int, position: Int, column: Int): TokenGenerationResult = {
    matchResult._2 match {
      case None => {
        (None, line, position, column)
      }
      case Some(TokenTypesImpl.WHITESPACE) => {
        (None, line, matchResult._1.length + position, matchResult._1.length + column)
      }
      case Some(TokenTypesImpl.EOL) => {
        (None, line + 1, matchResult._1.length + position, 0)
      }
      case Some(x) => {
        (Some(new Token(x, position, position + matchResult._1.length, new LexicalRange(column, line, column + matchResult._1.length, line ))), line, matchResult._1.length + position, matchResult._1.length + column)
      }
    }
  }
}

