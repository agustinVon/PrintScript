package lexer

import tokens.TokenTypesImpl
import org.austral.ingsis.printscript.common.TokenType

case class Matches() {
  def getMatches: List[(TokenType, LexerMatcher)] = List(
    (TokenTypesImpl.LET, LexerMatcherImpl.fromRegex(TokenTypesImpl.LET, "let")),
    (TokenTypesImpl.PRINTLN, LexerMatcherImpl.fromRegex(TokenTypesImpl.PRINTLN, "println")),

    (TokenTypesImpl.PLUS, LexerMatcherImpl.fromRegex(TokenTypesImpl.PLUS, "[+]")),
    (TokenTypesImpl.MINUS, LexerMatcherImpl.fromRegex(TokenTypesImpl.MINUS, "[-]")),
    (TokenTypesImpl.TIMES, LexerMatcherImpl.fromRegex(TokenTypesImpl.TIMES, "[*]")),
    (TokenTypesImpl.DIVIDEDBY, LexerMatcherImpl.fromRegex(TokenTypesImpl.DIVIDEDBY, "[/]")),
    (TokenTypesImpl.ASSIGNMENT, LexerMatcherImpl.fromRegex(TokenTypesImpl.ASSIGNMENT, "[=]")),
    (TokenTypesImpl.OPENPAREN, LexerMatcherImpl.fromRegex(TokenTypesImpl.OPENPAREN, "[(]")),
    (TokenTypesImpl.CLOSEPAREN, LexerMatcherImpl.fromRegex(TokenTypesImpl.CLOSEPAREN, "[)]")),
    (TokenTypesImpl.NUMBER, LexerMatcherImpl.fromRegex(TokenTypesImpl.NUMBER, "-?[0-9.]+")),
    (
      TokenTypesImpl.STRING,
      LexerMatcherImpl.fromRegex(TokenTypesImpl.STRING, "\\\"([_a-zA-Z0-9 !\\\\/.])*\\\"|'([_a-zA-Z0-9 !\\\\/.])*'")
    ),
    (
      TokenTypesImpl.IDENTIFIER,
      LexerMatcherImpl.fromRegex(TokenTypesImpl.IDENTIFIER, "(?!(string|number|const)\\b)\\b[_a-zA-Z][_a-zA-Z0-9]{0,30}")
    ),
    (TokenTypesImpl.TYPESTRING, LexerMatcherImpl.fromRegex(TokenTypesImpl.TYPESTRING, "string")),
    (TokenTypesImpl.TYPENUMBER, LexerMatcherImpl.fromRegex(TokenTypesImpl.TYPENUMBER, "number")),
    (TokenTypesImpl.WHITESPACE, LexerMatcherImpl.fromRegex(TokenTypesImpl.WHITESPACE, " ")),
    (TokenTypesImpl.COLON, LexerMatcherImpl.fromRegex(TokenTypesImpl.COLON, "[:]")),
    (TokenTypesImpl.SEMICOLON, LexerMatcherImpl.fromRegex(TokenTypesImpl.SEMICOLON, "[;]")),
    (TokenTypesImpl.EOL, LexerMatcherImpl.fromRegex(TokenTypesImpl.EOL, "[\\n]"))
  )
}
