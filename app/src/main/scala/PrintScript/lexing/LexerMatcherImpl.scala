package PrintScript.lexing

import org.austral.ingsis.printscript.common.TokenType

import java.util.regex.{Matcher, Pattern}

case class LexerMatcherImpl (val pattern:Pattern) {
  def getMatcher(input: String): Matcher = pattern.matcher(input)
}

object LexerMatcherImpl {
  def fromMatchers(matchers: List[LexerMatcher]):LexerMatcherImpl = {
    LexerMatcherImpl(Pattern.compile(matchers.map(matcher => matcher.getPattern.toString).mkString("|")))
  }

  def fromRegex(TType: TokenType, regex: String): LexerMatcherImpl = {
    LexerMatcherImpl(Pattern.compile(String.format("(?<%s>%s)", TType, regex)))
  }
}
