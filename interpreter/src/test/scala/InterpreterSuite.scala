import exceptions.{InvalidOperationException, TypeMismatchException}
import interpreter.InterpreterImpl
import lexer.LexerImpl
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertThrows
import parser.ParserImpl
import sources.StringProgramSource

import scala.jdk.CollectionConverters._

class InterpreterSuite {

  @Test
  def declarationIntegerValueShouldBeStored(): Unit = {
    val content = "let x:number = 8;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 8)
  }

  @Test
  def declarationStringValueShouldBeStored(): Unit = {
    val content = "let x:string = \"testing\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == "testing")
  }

  @Test
  def stringConcatenationShouldBeStored(): Unit = {
    val content = "let x:string = \"hello \"; \n" +
      "let y: string = \"world\"; \n" +
      "let z: string = x + y;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("z").get == "hello world")
  }

  @Test
  def stringAndIntegerConcatenationShouldBeStored(): Unit = {
    val content = "let x: string = \"num \"; \n" +
      "let y: number = 10;"+
      "let z: string = x + y;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("z").get == "num 10")
  }


  @Test
  def twoNumbersShouldBeAbleToSum(): Unit = {
    val content = "let x: number = 5 + 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 7)
  }

  @Test
  def twoNumbersShouldBeAbleToSubstract(): Unit = {
    val content = "let x: number = 5 - 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 3)
  }

  @Test
  def twoNumbersShouldBeAbleToMultiply(): Unit = {
    val content = "let x: number = 5 * 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 10)
  }

  @Test
  def twoNumbersShouldBeAbleToDivide(): Unit = {
    val content = "let x: number = 10 / 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = 10 - \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = 10 / \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = 10 * \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = \"test\" - 10;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = \"test\" / 10;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = \"test\" * 10;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def twoStringsShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = \"test\" * \"error\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def twoStringsShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = \"test\" - \"error\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def twoStringsShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = \"test\" / \"error\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast))
  }

  @Test
  def stringAssignationShouldNotBeAbleToStoreANumber(): Unit = {
    val content = "let x: string = 5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[TypeMismatchException], () => interpreter.interpret(ast))
  }

  @Test
  def numberAssignationShouldNotBeAbleToStoreAString(): Unit = {
    val content = "let x: number = \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[TypeMismatchException], () => interpreter.interpret(ast))
  }

  @Test
  def printLnShouldBeAbleToPrintAVariable(): Unit = {
    val content = "let x: number = 5;" +
      "println(x);"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(true)
  }

  @Test
  def numberVariableShouldBeAbleToStoreNewValue(): Unit = {
    val content = "let x: number = 5;" +
      "x = 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 2)
  }

  @Test
  def stringVariableShouldBeAbleToStoreNewValue(): Unit = {
    val content = "let x: string = \"test\";" +
      "x = \"hello world\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == "hello world")
  }

  @Test
  def variableShouldBeAssignedToAnotherVariableValue(): Unit = {
    val content = "let x: number = 5;" +
      "let y: number = x; "
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("y").get == 5)
  }

}
