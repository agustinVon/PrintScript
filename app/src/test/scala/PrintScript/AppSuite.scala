/*
 * This Scala Testsuite was generated by the Gradle 'init' task.
 */
package PrintScript

import PrintScript.Tester.content
import lexer._
import parser._
import interpreter._
import org.junit.jupiter.api.Test

import scala.jdk.CollectionConverters._

object Tester extends App {
  //val content = "let txt:string = \"\";" +
    "let n:number = 2;" +
    "println(4+(5+6));"

  val content = "let x:number = 7 * 2;" +
    "let y: string = \"hola\" + x + x + \"chau\"; \n" +
    "let w: string = 4;" +
    "println(y);"


  val tokens = LexerImpl().lex(StringProgramSource(content))
  val ast = new ParserImpl().parse(content, tokens.asJava)
  val interpreter = new InterpreterImpl
  interpreter.validate(ast)
  /*
  var variables: Map[String, Option[Any]] = Map()
  println(variables.get("hola"))
  println(2 + "holaa")

   */

}

class AppSuite{
  @Test
  def appHasAGreeting(): Unit = {

  }

  @Test
  def declarationIntegerValueShouldBeStored() = {
    val content = "let x:number = 2*4;"
    val tokens = LexerImpl().lex(StringProgramSource(content))
    val ast = new ParserImpl().parse(content, tokens.asJava)
    val interpreter = new InterpreterImpl()
    interpreter.interpret(ast)
    assert(interpreter.getMemory()("x").get == 8)
  }
}
