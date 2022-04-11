package PrintScript.interpreter

import ast.ASTree

trait Interpreter {
  def interpret(ast:ASTree)
  def validate(ast:ASTree)
  def getMemory():Map[String, Option[Any]]
}
