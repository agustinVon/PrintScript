package PrintScript.lexing

import java.util.regex.{Matcher, Pattern}

trait LexerMatcher {

  def getPattern: Pattern

  def getMatcher(input: String): Matcher

}
