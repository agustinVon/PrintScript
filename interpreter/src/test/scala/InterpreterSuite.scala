import interpreter.InterpreterImpl
import lexer.LexerImpl
import org.junit.jupiter.api.Test
import parser.ParserImpl
import sources.StringProgramSource
import scala.jdk.CollectionConverters._

class InterpreterSuite {

  @Test
  def declarationIntegerValueShouldBeStored(): Unit = {
    val content = "let x:number = 8;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = new ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = new InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 8)
  }
}
