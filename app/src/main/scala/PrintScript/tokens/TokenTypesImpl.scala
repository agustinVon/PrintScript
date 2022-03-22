package PrintScript.tokens

import org.austral.ingsis.printscript.common.TokenType

object TokenTypesImpl{

  abstract class ComparableToken() extends TokenType{
    override def equals(tokenType:TokenType): Boolean = tokenType.getType == this.getType
  }

  case object LET extends ComparableToken {
    override def getType:String = "LET"
}
  case object PRINTLN extends ComparableToken {
    override def getType: String = "PRINTLN"
  }
  case object TYPESTRING extends ComparableToken {
    override def getType: String = "TYPESTRING"
  }
  case object TYPENUMBER extends ComparableToken {
    override def getType: String = "TYPENUMBER"
  }
  case object PLUS extends ComparableToken {
    override def getType: String = "PLUS"
  }
  case object MINUS extends ComparableToken {
    override def getType: String = "MINUS"
  }
  case object TIMES extends ComparableToken {
    override def getType: String = "TIMES"
  }
  case object DIVIDEDBY extends ComparableToken {
    override def getType: String = "DIVIDEDBY"
  }
  case object ASSIGNMENT extends ComparableToken {
    override def getType: String = "ASSIGNMENT"
  }
  case object OPENPAREN extends ComparableToken {
    override def getType: String = "OPENPAREN"
  }
  case object CLOSEPAREN extends ComparableToken {
    override def getType: String = "CLOSEPAREN"
  }
  case object NUMBER extends ComparableToken {
    override def getType: String = "NUMBER"
  }
  case object STRING extends ComparableToken {
    override def getType: String = "STRING"
  }
  case object IDENTIFIER extends ComparableToken {
    override def getType: String = "IDENTIFIER"
  }
  case object WHITESPACE extends ComparableToken {
    override def getType: String = "WHITESPACE"
  }
  case object EOF extends ComparableToken {
    override def getType: String = "EOF"
  }
  case object COLON extends ComparableToken {
    override def getType: String = "COLON"
  }
  case object SEMICOLON extends ComparableToken {
    override def getType: String = "SEMICOLON"
  }

  case object EOL extends ComparableToken {
    override def getType: String = "EOL"
  }
  case class UnknownToken(name: String) extends TokenType {
    override def getType:String = "UNKNOWN"
    override def equals(tokenType: TokenType): Boolean = false
}
}