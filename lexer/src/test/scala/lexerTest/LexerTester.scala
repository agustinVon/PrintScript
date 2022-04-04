package lexerTest

import lexer.LexerImpl

object LexerTester extends App {
  val lexer = new LexerImpl()
  val tokens = lexer.lex("let str:string = '2';\nlet b:number = 2;")
  println(tokens)
}
