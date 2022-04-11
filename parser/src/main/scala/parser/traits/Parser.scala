package parser.traits

import ast.ASTree
import org.austral.ingsis.printscript.common.Token
import sources.ProgramSource

trait Parser {
  def parse(content: ProgramSource, tokens: List[Token]): ASTree
}
