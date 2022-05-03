import exceptions.{ConstantAlreadyDeclaredException, ConstantMustBeInitializedWithValueException, ConstantValueCannotBeModifiedException, InvalidOperationException, TypeMismatchException, VariableAlreadyDeclaredException}
import interpreter.{InterpreterImpl, PrintScriptInput, PrintScriptInputTest, PrintScriptPrinter}
import lexer.LexerImpl
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.Assertions.assertThrows
import parser.ParserImpl
import sources.StringProgramSource


class InterpreterSuite {

  @Test
  def declarationIntegerValueShouldBeStored(): Unit = {
    val content = "let x:number = 8;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 8)
  }

  @Test
  def declarationStringValueShouldBeStored(): Unit = {
    val content = "let x:string = \"testing\";"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == "testing")
  }

  @Test
  def stringConcatenationShouldBeStored(): Unit = {
    val content = "let x:string = \"hello \"; \n" +
      "let y: string = \"world\"; \n" +
      "let z: string = x + y;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("z").get == "hello world")
  }

  @Test
  def stringAndIntegerConcatenationShouldBeStored(): Unit = {
    val content = "let x: string = \"num \"; \n" +
      "let y: number = 10;"+
      "let z: string = x + y;" +
      "println(z);"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("z").get == "num 10")
  }


  @Test
  def twoNumbersShouldBeAbleToSum(): Unit = {
    val content = "let x: number = 5 + 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 7)
  }

  @Test
  def twoNumbersShouldBeAbleToSubstract(): Unit = {
    val content = "let x: number = 5 - 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 3)
  }

  @Test
  def twoNumbersShouldBeAbleToMultiply(): Unit = {
    val content = "let x: number = 5 * 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 10)
  }

  @Test
  def twoNumbersShouldBeAbleToDivide(): Unit = {
    val content = "let x: number = 10 / 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = 10 - \"test\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = 10 / \"test\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def aNumberAndAStringShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = 10 * \"test\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = \"test\" - 10;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = \"test\" / 10;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def aStringAndANumberShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = \"test\" * 10;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def twoStringsShouldNotBeAbleToMultiply(): Unit = {
    val content = "let x: number = \"test\" * \"error\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def twoStringsShouldNotBeAbleToSubstract(): Unit = {
    val content = "let x: number = \"test\" - \"error\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def twoStringsShouldNotBeAbleToDivide(): Unit = {
    val content = "let x: number = \"test\" / \"error\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[InvalidOperationException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def stringAssignationShouldNotBeAbleToStoreANumber(): Unit = {
    val content = "let x: string = 5;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[TypeMismatchException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def numberAssignationShouldNotBeAbleToStoreAString(): Unit = {
    val content = "let x: number = \"test\";"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[TypeMismatchException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def printLnShouldBeAbleToPrintAVariable(): Unit = {
    val content = "let x: number = 5;" +
      "println(x);"
    testInitializer(content)
    assert(true)
  }

  @Test
  def numberVariableShouldBeAbleToStoreNewValue(): Unit = {
    val content = "let x: number = 5;" +
      "x = 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 2)
  }

  @Test
  def stringVariableShouldBeAbleToStoreNewValue(): Unit = {
    val content = "let x: string = \"test\";" +
      "x = \"hello world\";"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == "hello world")
  }

  @Test
  def variableShouldBeAssignedToAnotherVariableValue(): Unit = {
    val content = "let x: number = 5;" +
      "let y: number = x; "
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("y").get == 5)
  }

  @Test
  def variableDeclarationWithNoValueShouldStoreVariableWithDefaultValue(): Unit = {
    val content = "let x: boolean;" +
      "x=true;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == true)
  }

  @Test
  def stringWithPointTest(): Unit = {
    val content = "let x: string = \"esta es una oracion con .\";"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == "esta es una oracion con .")
  }

  @Test
  def intNumberTest(): Unit = {
    val content = "let x: number = 1;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 1)
  }

  @Test
  def decimalNumberTest(): Unit = {
    val content = "let x: number = 1.0;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 1.0)
  }

  @Test
  def stringIntConcatenation(): Unit = {
    val content = "let x: string = \"test \" + 1;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == "test 1")
  }

  @Test
  def stringDecimalConcatenation(): Unit = {
    val content = "let x: string = \"test \" + 1.0;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == "test 1.0")
  }

  @Test
  def intWithDecimalShouldSum(): Unit = {
    val content = "let x: number = 4.5 + 3;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 7.5)
  }

  @Test
  def intWithDecimalShouldSubstract(): Unit = {
    val content = "let x: number = 4.5 - 3;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 1.5)
  }

  @Test
  def intWithDecimalShouldMultiply(): Unit = {
    val content = "let x: number = 4.5 * 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 9)
  }

  @Test
  def intWithDecimalShouldDivide(): Unit = {
    val content = "let x: number = 12.5 / 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 6.25)
  }

  @Test
  def decimalWithIntShouldSum(): Unit = {
    val content = "let x: number = 2 + 2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 4.5)
  }

  @Test
  def decimalWithIntShouldSubstract(): Unit = {
    val content = "let x: number = 3 - 2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 0.5)
  }

  @Test
  def decimalWithIntShouldMultiply(): Unit = {
    val content = "let x: number = 2 * 2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def decimalWithIntShouldDivide(): Unit = {
    val content = "let x: number = 10 / 2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 4)
  }

  @Test
  def twoDecimalsShouldSum(): Unit = {
    val content = "let x: number = 2.5 + 2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def twoDecimalsShouldSubstract(): Unit = {
    val content = "let x: number = 5.5 - 2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 3)
  }

  @Test
  def twoDecimalsShouldMultiply(): Unit = {
    val content = "let x: number = 1.5 * 0.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 0.75)
  }

  @Test
  def twoDecimalsShouldDivide(): Unit = {
    val content = "let x: number = 2.5 / 0.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == 5)
  }

  @Test
  def aDecimalAndAStringShouldConcatenate(): Unit = {
    val content = "let x: string = 2.5 + \" test\";"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("x").get == "2.5 test")
  }

  @Test
  def aStringAndADecimalShouldConcatenate(): Unit = {
    val content = "let numberMy: string =  \"test \" +2.5;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("numberMy").get == "test 2.5")
  }

  @Test
  def precedenceTest1(): Unit = {
    val content = "let numberMy: number = 2 ;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("numberMy").get == 2)
  }

  @Test
  def ifTest(): Unit = {
    val content = "let numberMy: number = 2;" +
      "let value: boolean = true;" +
      "if (true){" +
      "numberMy = 3;" +
      "};"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("numberMy").get == 3)
  }

  private def testInitializer(content: String) = {
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput())
    interpreter
  }

  @Test
  def ifElseTest(): Unit = {
    val content = "let numberMy: number = 2;" +
      "let value: boolean = true;" +
      "if (false){" +
      "numberMy = 3;" +
      "}else{" +
      "numberMy = 5;};"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("numberMy").get == 5)
  }

  @Test
  def constAsignationShouldStoreValue(): Unit = {
    val content = "const numberMy: number = 2;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("numberMy").get == 2)
  }

  @Test
  def constAsignationWithoutValueShouldRaiseError(): Unit = {
    val content = "const numberMy: number;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[ConstantMustBeInitializedWithValueException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def constShouldRaiseErrorWhenTriedToModifyValue(): Unit = {
    val content = "const numberMy: number = 4;" +
      "numberMy = 5;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[ConstantValueCannotBeModifiedException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def variableAssignationShouldRaiseAnErrorWhenConstWithSameIdentifierWasPreviouslyInitialized(): Unit = {
    val content = "const numberMy: number = 4;" +
      "let numberMy:number = 5;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[ConstantAlreadyDeclaredException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def constAssignationShouldRaiseAnErrorWhenVariableWithSameIdentifierWasPreviouslyInitialized(): Unit = {
    val content = "let numberMy: number = 4;" +
      "const numberMy:number = 5;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[VariableAlreadyDeclaredException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def variableAssignationShouldRaiseAnErrorWhenVariableWithSameIdentifierWasPreviouslyInitialized(): Unit = {
    val content = "let numberMy: number = 4;" +
      "let numberMy:number = 5;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[VariableAlreadyDeclaredException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def constAssignationShouldRaiseAnErrorWhenConstWithSameIdentifierWasPreviouslyInitialized(): Unit = {
    val content = "const numberMy: number = 4;" +
      "const numberMy:number = 5;"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    assertThrows(classOf[ConstantAlreadyDeclaredException], () => interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput()))
  }

  @Test
  def constShouldBeInitializedWithVariableValue(): Unit = {
    val content = "let numberMy: number = 4;" +
      "const constNumber:number = numberMy;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("constNumber").get == 4)
  }

  @Test
  def variableShouldBeInitializedWithConstValue(): Unit = {
    val content = "const constNumber: number = 4;" +
      "let variableNumber:number = constNumber;"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("variableNumber").get == 4)
  }

  @Test
  def ifTestWithBooleanVariable(): Unit = {
    val content = "const value: boolean = true;" +
      "let result: string;" +
      "if(value){" +
      "result = \"success\" ;" +
      "}else{" +
      "result = \"failure\" ;" +
      "};"
    val interpreter: InterpreterImpl = testInitializer(content)
    assert(interpreter.getMemory()("result").get.equals("success"))
  }

  @Test
  def readInputFromConst(): Unit = {
    val content = "const hint: string = \"ingrese un texto\";"+
      "const myText: string = readInput(hint);" +
      "println(myText);"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInputTest("TextInputResult"))
    assert(interpreter.getMemory()("myText").get.equals("TextInputResult"))
  }

  @Test
  def readInput(): Unit = {
    val content = "const myText: string = readInput(\"ingrese un texto\");" +
      "println(myText);"
    val tokens = LexerImpl("1.1").lex(StringProgramSource(content))
    val ast = ParserImpl().parse(StringProgramSource(content), tokens)
    val interpreter = InterpreterImpl()
    interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInputTest("TextInputResult"))
    assert(interpreter.getMemory()("myText").get.equals("TextInputResult"))
  }

}
