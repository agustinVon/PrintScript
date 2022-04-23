package parser.traits

import ast.{ASTree, Expression}
import org.austral.ingsis.printscript.common.TokenConsumer

trait SectionParser {
  def parse(consumer: TokenConsumer): ASTree
  def canBeParsed(consumer: TokenConsumer): Boolean
}

trait ExpressionSectionParser extends SectionParser {
  override def parse(consumer: TokenConsumer): Expression
}

trait OperationParser {
  def parse(exp: Expression, consumer: TokenConsumer): Expression
  def canBeParsed(consumer: TokenConsumer): Boolean
}
