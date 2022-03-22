package parser

import org.austral.ingsis.printscript.common.Token


sealed trait ASTree
case class Leaf(value: Token) extends ASTree
case class Branch(left: ASTree, right: ASTree) extends ASTree
