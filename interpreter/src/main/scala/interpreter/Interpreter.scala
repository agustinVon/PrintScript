package interpreter

import ast.ASTree

trait Interpreter {
  def interpret(ast: ASTree, displayMethod: (String) => Unit, stringInput: () => String):Unit
  def validate(ast: ASTree, displayMethod: (String) => Unit):Unit
  def getMemory(): Map[String, Option[Any]]
}
