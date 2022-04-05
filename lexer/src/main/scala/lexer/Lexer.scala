package lexer

import org.austral.ingsis.printscript.common.Token

trait Lexer {
  def lex(source: ProgramSource): List[Token]
}
