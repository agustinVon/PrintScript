import org.austral.ingsis.printscript.common.Token

trait PrintScriptParser {
  def parse(list: List[Token]) =
    println(list)
}