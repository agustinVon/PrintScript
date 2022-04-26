/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package PrintScript
import interpreter.{InterpreterImpl, PrintScriptInput, PrintScriptPrinter}
import lexer.LexerImpl
import parser.ParserImpl
import parser.exceptions.{ExpectedEndOfLineException, ExpressionExpectedException}
import sources.{FileProgramSource, StringProgramSource}

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Using}

object App {
  private val lexer       = LexerImpl()
  private val parser      = ParserImpl()
  private val interpreter = InterpreterImpl()

  def main(args: Array[String]): Unit = {
    displayIntro(println)
    val path: String = displayPathOption(println)
    val option: Int  = displayMenu(println)

    if (option == 1) {
      try {
        interpret(FileProgramSource(path), println)
      } catch {
        case e: ExpectedEndOfLineException =>
          println("ERROR\ncolumn: " + e.position + " line: " + e.line)
          println(e.getMessage)
        case e: ExpressionExpectedException =>
          println("ERROR\ncolumn: " + e.position + " line: " + e.line)
          println(e.getMessage)
        case e: Exception =>
          println(e.getMessage)
      }
    }

    if (option == 2) {
      try {
        validate(FileProgramSource(path), println)
      } catch {
        case e:Exception =>
          println("Validation failed")
          println(e.getMessage)
      }
    }
  }

  def interpret(source: FileProgramSource, displayMethod: (String) => Unit): Unit = {
    val tokens = lexer.lex(source)
    val ast    = parser.parse(source, tokens)
    interpreter.interpret(ast, PrintScriptPrinter(), PrintScriptInput())
  }

  def validate(source: FileProgramSource, displayMethod: (String) => Unit): Unit = {
    val tokens = lexer.lex(source)
    val ast    = parser.parse(source, tokens)
    interpreter.validate(ast)
  }

  def displayIntro(displayMethod: (String) => Unit): Unit = {
    displayMethod(" _____      _       _    _____           _       _   ")
    displayMethod("|  __ \\    (_)     | |  / ____|         (_)     | |  ")
    displayMethod("| |__) | __ _ _ __ | |_| (___   ___ _ __ _ _ __ | |_ ")
    displayMethod("|  ___/ '__| | '_ \\| __|\\___ \\ / __| '__| | '_ \\| __|")
    displayMethod("| |   | |  | | | | | |_ ____) | (__| |  | | |_) | |_ ")
    displayMethod("|_|   |_|  |_|_| |_|\\__|_____/ \\___|_|  |_| .__/ \\__|")
    displayMethod("                                          | |        ")
    displayMethod("                                          |_|        ")
    displayMethod("")
    displayMethod("_____________________________________________________")
    displayMethod("")
    displayMethod("")
  }

  def displayPathOption(displayMethod: (String) => Unit): String = {
    displayMethod("Path to file: ")
    displayMethod("")
    scala.io.StdIn.readLine()
  }

  def displayMenu(displayMethod: (String) => Unit): Int = {
    displayMethod("1. Interpret")
    displayMethod("2. Validate")
    displayMethod("")
    val option = scala.io.StdIn.readInt()
    if (option == 1 || option == 2) {
      option
    } else {
      displayMenu(displayMethod)
    }
  }

}
