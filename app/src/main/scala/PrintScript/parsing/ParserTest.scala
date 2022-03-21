package PrintScript.parsing

import PrintScript.lexing.LexTester.lexer
import PrintScript.lexing.LexerImpl

object ParserTest extends App {
  val content = "let str:string = '2';let b:number = 2;"
  val lexer = new LexerImpl()
  val tokens = lexer.lex(content)
  val parser = new ParserImpl()
  val tree = parser.parse(content, tokens)
}
