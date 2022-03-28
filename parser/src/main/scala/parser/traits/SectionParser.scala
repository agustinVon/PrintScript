package parser.traits

import ast.ASTree
import org.austral.ingsis.printscript.common.TokenConsumer

trait SectionParser {
  def parse(consumer: TokenConsumer, tree: Option[ASTree]): ASTree
  def canBeParsed(consumer: TokenConsumer): Boolean
}
