package PrintScript.interpreter

import ast.ASTree

trait Interpreter {
  def interpret(ast:ASTree)
}
