package lexer

import org.austral.ingsis.printscript.common.Token

import java.util.regex.{Matcher, Pattern}

trait LexerMatcher {

  def getPattern: Pattern

  def getMatcher(input: String): Matcher
}
