package PrintScript.lexing

import org.austral.ingsis.printscript.common.Token

trait Lexer {
  def lex(source: String): List[Token]
}
