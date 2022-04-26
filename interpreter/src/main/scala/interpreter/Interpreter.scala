package interpreter

import ast.ASTree

trait Interpreter {
  def interpret(ast: ASTree, displayMethod: DisplayMethod, input: InputMethod): Unit
  def validate(ast: ASTree): Unit
  def getMemory(): Map[String, Option[Any]]
}
