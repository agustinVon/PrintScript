import lexer._
import org.austral.ingsis.printscript.common.Token
import org.junit.jupiter.api.Test
import tokens.TokenTypesImpl

class LexerSuite {

  @Test
  def testLexerMatcherImplFromRegexPattern() = {
    val regex = "[\\n]"
    val matcher = LexerMatcherImpl.fromRegex(TokenTypesImpl.EOL,regex)
    assert("(?<EOL>[\\n])".equals(matcher.getPattern.toString))
  }

  @Test
  def testLexerMatcherImplFromMatcherPattern() = {
    val matcher1 = LexerMatcherImpl.fromRegex(TokenTypesImpl.EOL,"[\\n]")
    val matcher2 = LexerMatcherImpl.fromRegex(TokenTypesImpl.LET,"let")
    val matcher3 = LexerMatcherImpl.fromRegex(TokenTypesImpl.PRINTLN,"println")
    val mainmatcher = LexerMatcherImpl.fromMatchers(List(matcher1, matcher2, matcher3))
    println(mainmatcher.getPattern.toString)
    assert("(?<EOL>[\\n])|(?<LET>let)|(?<PRINTLN>println)".equals(mainmatcher.getPattern.toString))
  }

  @Test
  def testMatches() = {
    val matches = Matches().getMatches
    val letMatcher = LexerMatcherImpl.fromRegex(TokenTypesImpl.LET,"let")
    println(matches.head._2.getPattern)
    assert(matches.head._2.getPattern.toString.equals(letMatcher.getPattern.toString))
  }

  @Test
  def testSingleLineLexTokenCount() = {
    val source = StringProgramSource("let num:number = 4;")
    val tokens = LexerImpl().lex(source)
    assert(tokens.length == 8)
  }

  @Test
  def testMultipleLineLexTokenCount() = {
    val source = StringProgramSource("let num:number = 4;\n let num2:number = 6;")
    val tokens = LexerImpl().lex(source)
    assert(tokens.length == 15)
  }

  @Test
  def testSingleLineLexTokens() = {
    val source = StringProgramSource("let num:number = 4;")
    val tokens:List[Token] = LexerImpl().lex(source)
    println(tokens)
    assert(tokens.head.getType == TokenTypesImpl.LET
      && tokens(1).getType == TokenTypesImpl.IDENTIFIER
      && tokens(2).getType == TokenTypesImpl.COLON
      && tokens(3).getType == TokenTypesImpl.TYPENUMBER
      && tokens(4).getType == TokenTypesImpl.ASSIGNMENT
      && tokens(5).getType == TokenTypesImpl.NUMBER
      && tokens(6).getType == TokenTypesImpl.SEMICOLON
      && tokens(7).getType == TokenTypesImpl.EOF)
  }

}
