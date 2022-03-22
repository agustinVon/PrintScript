/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package PrintScript

import lexer.LexerImpl
import parser.ParserImpl

object App {
  def main(args: Array[String]): Unit = {
    val content = "let str:string = '2';let b:number = 2;"
    val lexer = new LexerImpl()
    val tokens = lexer.lex(content)
    val parser = new ParserImpl()
    val tree = parser.parse(content, tokens)
  }

  def greeting(): String = "Hello, world!"
}
