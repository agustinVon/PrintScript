package parser.traits

import ast.ASTree
import org.austral.ingsis.printscript.common.Token

trait Parser {
  def parse(content: String, tokens: java.util.List[Token]): ASTree
}
