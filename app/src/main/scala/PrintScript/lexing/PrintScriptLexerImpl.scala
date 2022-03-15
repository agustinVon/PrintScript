package PrintScript.lexing
import org.austral.ingsis.printscript.common.Token

object LexTester extends App {
  val lexer = new LexerImpl()
  val tokens = lexer.lex("var str:String = '2';\n val b:Number = 2;")
  println(tokens)
}
