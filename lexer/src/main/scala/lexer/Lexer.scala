package lexer

import org.austral.ingsis.printscript.common.Token
import sources.ProgramSource

trait Lexer {
  def lex(source: ProgramSource): List[Token]
  def setVersion(version: String): Unit
}
