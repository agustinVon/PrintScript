package PrintScript.lexing
import org.austral.ingsis.printscript.common.Token

object LexTester extends App {
  val lexer = new LexerImpl()
  val tokens = lexer.lex("let str:string = '2';\n let b:number = 2;")
  println(tokens)
}
