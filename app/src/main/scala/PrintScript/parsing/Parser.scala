package PrintScript.parsing

import org.austral.ingsis.printscript.common.Token

trait Parser {
  def parse(content:String, list: List[Token]): ASTree
}
