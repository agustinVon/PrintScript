package ast

import org.austral.ingsis.printscript.common.Token
import org.austral.ingsis.printscript.parser.Content


sealed trait ASTree
case class Leaf(value: Content[String]) extends ASTree
case class Branch(branches: List[ASTree], node: Content[String]) extends ASTree
case class StringBranch(branches: List[ASTree], node: String) extends ASTree
