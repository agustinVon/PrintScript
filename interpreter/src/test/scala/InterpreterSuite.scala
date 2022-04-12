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
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 8)
  }

  @Test
  def declarationStringValueShouldBeStored(): Unit = {
    val content = "let x:string = \"testing\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
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
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("z").get == "hello world")
  }

  @Test
  def stringAndIntegerConcatenationShouldBeStored(): Unit = {
    val content = "let x: string = \"num \"; \n" +
      "let y: number = 10;"+
      "let z: string = x + y;" +
      "println(z);"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("z").get == "num 10")
  }


  @Test
  def twoNumbersShouldBeAbleToSum(): Unit = {
    val content = "let x: number = 5 + 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 7)
  }

  @Test
  def twoNumbersShouldBeAbleToSubstract(): Unit = {
    val content = "let x: number = 5 - 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 3)
  }

  @Test
  def twoNumbersShouldBeAbleToMultiply(): Unit = {
    val content = "let x: number = 5 * 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 10)
  }

  @Test
  def twoNumbersShouldBeAbleToDivide(): Unit = {
    val content = "let x: number = 10 / 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = 10 - \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = 10 / \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = 10 * \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = \"test\" - 10;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = \"test\" / 10;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = \"test\" * 10;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def twoStringsShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = \"test\" * \"error\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def twoStringsShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = \"test\" - \"error\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def twoStringsShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = \"test\" / \"error\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, println))
  }

  @Test
  def stringAssignationShouldNotBeAbleToStoreANumber(): Unit = {
    val content = "let x: string = 5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[TypeMismatchException], () => interpreter.interpret(ast, println))
  }

  @Test
  def numberAssignationShouldNotBeAbleToStoreAString(): Unit = {
    val content = "let x: number = \"test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[TypeMismatchException], () => interpreter.interpret(ast, println))
  }

  @Test
  def printLnShouldBeAbleToPrintAVariable(): Unit = {
    val content = "let x: number = 5;" +
      "println(x);"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(true)
  }

  @Test
  def numberVariableShouldBeAbleToStoreNewValue(): Unit = {
    val content = "let x: number = 5;" +
      "x = 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 2)
  }

  @Test
  def stringVariableShouldBeAbleToStoreNewValue(): Unit = {
    val content = "let x: string = \"test\";" +
      "x = \"hello world\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == "hello world")
  }

  @Test
  def variableShouldBeAssignedToAnotherVariableValue(): Unit = {
    val content = "let x: number = 5;" +
      "let y: number = x; "
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("y").get == 5)
  }

  @Test
  def declarationWithNoValueShouldStoreVariableWithDefaultValue(): Unit = {
    val content = "let x: number;" +
      "x=5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def stringWithPointTest(): Unit = {
    val content = "let x: string = \"esta es una oracion con .\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == "esta es una oracion con .")
  }

  @Test
  def intNumberTest(): Unit = {
    val content = "let x: number = 1;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 1)
  }

  @Test
  def decimalNumberTest(): Unit = {
    val content = "let x: number = 1.0;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 1.0)
  }

  @Test
  def stringIntConcatenation(): Unit = {
    val content = "let x: string = \"test \" + 1;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == "test 1")
  }

  @Test
  def stringDecimalConcatenation(): Unit = {
    val content = "let x: string = \"test \" + 1.0;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == "test 1.0")
  }

  @Test
  def intWithDecimalShouldSum(): Unit = {
    val content = "let x: number = 4.5 + 3;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 7.5)
  }

  @Test
  def intWithDecimalShouldSubstract(): Unit = {
    val content = "let x: number = 4.5 - 3;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 1.5)
  }

  @Test
  def intWithDecimalShouldMultiply(): Unit = {
    val content = "let x: number = 4.5 * 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 9)
  }

  @Test
  def intWithDecimalShouldDivide(): Unit = {
    val content = "let x: number = 12.5 / 2;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 6.25)
  }

  @Test
  def decimalWithIntShouldSum(): Unit = {
    val content = "let x: number = 2 + 2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 4.5)
  }

  @Test
  def decimalWithIntShouldSubstract(): Unit = {
    val content = "let x: number = 3 - 2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 0.5)
  }

  @Test
  def decimalWithIntShouldMultiply(): Unit = {
    val content = "let x: number = 2 * 2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def decimalWithIntShouldDivide(): Unit = {
    val content = "let x: number = 10 / 2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 4)
  }

  @Test
  def twoDecimalsShouldSum(): Unit = {
    val content = "let x: number = 2.5 + 2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def twoDecimalsShouldSubstract(): Unit = {
    val content = "let x: number = 5.5 - 2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 3)
  }

  @Test
  def twoDecimalsShouldMultiply(): Unit = {
    val content = "let x: number = 1.5 * 0.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 0.75)
  }

  @Test
  def twoDecimalsShouldDivide(): Unit = {
    val content = "let x: number = 2.5 / 0.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def aDecimalAndAStringShouldConcatenate(): Unit = {
    val content = "let x: string = 2.5 + \" test\";"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == "2.5 test")
  }

  @Test
  def aStringAndADecimalShouldConcatenate(): Unit = {
    val content = "let x: string =  \"test \" +2.5;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, println)
    assert(interpreter.getMemory()("x").get == "test 2.5")
  }

}
