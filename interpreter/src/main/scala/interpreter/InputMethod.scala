package interpreter

trait InputMethod {
  def readInput(name: String, displayMethod: DisplayMethod): String
}
