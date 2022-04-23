package ast

import org.austral.ingsis.printscript.common.TokenType
import org.austral.ingsis.printscript.parser.Content

sealed trait ASTree
case class Root(sentences: List[ASTree])                                                             extends ASTree
case class Declaration(declaration: Content[String], id: Content[String], declType: Content[String]) extends ASTree
sealed trait Expression                                                                              extends ASTree
case class LiteralString(value: Content[String])                                                     extends Expression
case class LiteralNumber(value: Content[Double])                                                     extends Expression
case class Variable(value: Content[String])                                                          extends Expression
case class SumOrMinus(exp1: Expression, operator: Content[String], exp2: Expression) extends Expression
case class TimesOrDiv(exp1: Expression, operator: Content[String], exp2: Expression) extends Expression
case class PrintLn(function: Content[String], expression: Expression)                                extends ASTree
case class DeclarationAssignation(declaration: Declaration, assignation: Content[String], expression: Expression)
    extends ASTree
case class VariableAssignation(variable: Variable, assignation: Content[String], expression: Expression) extends ASTree
case class ParenExpression(expression: Expression)                                                       extends Expression
case class LiteralBoolean(value: Content[Boolean]) extends Expression
case class IfCodeBlock(condition: LiteralBoolean, codeBlock: ASTree) extends ASTree
case class IfElseCodeBlock(condition: LiteralBoolean, ifCodeBlock: ASTree, elseCodeBlock: ASTree) extends ASTree
case class ReadInput(function: Content[String]) extends ASTree
