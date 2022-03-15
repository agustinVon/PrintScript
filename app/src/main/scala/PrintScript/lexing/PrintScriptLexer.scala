package PrintScript.lexing

import org.austral.ingsis.printscript.common.Token

trait PrintScriptLexer {
  def lex(sourceText:String):List[Token]
}
