package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    // TODO: implement this method

    var currentChar : Option[Char] = Some(source.next())

    new Iterator[Token] {



      var hasNext = true

      def next = {

        while (currentChar.isDefined && currentChar.get.isWhitespace){
          currentChar = getNextChar(source)
        }

        if(currentChar.isEmpty){
          hasNext = false
          new Token(EOF)
        }else{
          val tuple = findToken(currentChar.get, source)
          currentChar = tuple._2
          tuple._1
        }
      }
    }
  }
  /*def validStart() : Boolean = lastSeenToken match {
      case Some(t) => previousIsWhitespace || !(t.isInstanceOf[ID] || t.isInstanceOf[INTLIT] || t.isInstanceOf[STRLIT])
      case None => true
    }*/
  def getToken(literal: String): Token = literal match {
    case "object" => new Token(OBJECT)
    case "class" => new Token(CLASS)
    case "def" => new Token(DEF)
    case "override" => new Token(OVERRIDE)
    case "var" => new Token(VAR)
    case "Unit" => new Token(UNIT)
    case "String" => new Token(STRING)
    case "extends" => new Token(EXTENDS)
    case "Int" => new Token(INT)
    case "Boolean" => new Token(BOOLEAN)
    case "while" => new Token(WHILE)
    case "if" => new Token(IF)
    case "else" => new Token(ELSE)
    case "true" => new Token(TRUE)
    case "false" => new Token(FALSE)
    case "this" => new Token(THIS)
    case "null" => new Token(NULL)
    case "new" => new Token(NEW)
    case "println" => new Token(PRINTLN)
    case _ => new ID(literal)
  }

  //Example: 123ABS is allowed in the Lexer but are gonna yield an error in the Parser
  //Return condition: nextChar is always gonna be the char after the token, e.q. 123A nextChar = A and token = 123
  def findToken(currentChar: Char, source: scala.io.BufferedSource): (Token, Option[Char]) = {

    var nextChar = getNextChar(source)

    val newToken = currentChar match {
      case digit if digit.isDigit && digit != '0' =>
        var integer = digit.asDigit

        while (nextChar.isDefined && nextChar.get.isDigit) {
          integer = integer * 10 + nextChar.get.asDigit
          nextChar = getNextChar(source)
        }

        new INTLIT(integer)

      case alfa if alfa.isLetter =>
        val buffer = new StringBuilder(alfa)

        while (nextChar.isDefined && (nextChar.get.isLetterOrDigit || nextChar.get == '_')) {
          buffer.append(nextChar.get)
          nextChar = getNextChar(source)
        }

        getToken(buffer.toString)

      case '"' =>
        val buffer = new StringBuilder
        while (nextChar.isDefined && nextChar.get != '"') {
          buffer.append(nextChar.get)
          nextChar = getNextChar(source)
          if (nextChar.isEmpty || nextChar.get == '\n')
            new Token(BAD) //TODO fix later
        }

        new STRLIT(buffer.toString())

      case ':' => new Token(COLON)
      case ';' => new Token(SEMICOLON)
      case '.' => new Token(DOT)
      case ',' => new Token(COMMA)
      case '!' => new Token(BANG)
      case '(' => new Token(LPAREN)
      case ')' => new Token(RPAREN)
      case '{' => new Token(LBRACE)
      case '}' => new Token(RBRACE)
      case '<' => new Token(LESSTHAN)
      case '+' => new Token(PLUS)
      case '-' => new Token(MINUS)
      case '*' => new Token(TIMES)
      case '&' =>
        nextChar match {
          case Some('&') =>
            nextChar = getNextChar(source)
            new Token(AND)

          case _ => new Token(BAD) //TODO fix later
        }
      case '|' =>
        nextChar match {
          case Some('|') =>
            nextChar = getNextChar(source)
            new Token(OR)

          case _ => new Token(BAD) //TODO fix later
        }

      case '/' => nextChar match {
        case Some('/') => ??? //TODO remove alla characters until new line or EOF and call the method findToken one more time, to get a token
        case Some('*') => ??? //TODO remove alla characters until next */ or EOF and call the method findToken one more time, to get a token
        case _ => new Token(DIV)
      }
      case '=' =>
        nextChar match {
          case Some('=') =>
            nextChar = getNextChar(source)
            new Token(EQUALS)

          case _ => new Token(EQSIGN)
        }
      case _ => new Token(BAD) //TODO fix consume char or have multiple bads until valid char
    }

    (newToken, nextChar)
  }

  def getNextChar(source: scala.io.BufferedSource): Option[Char] = {
    if (source.hasNext)
      Some(source.next())
    else
      None
  }
}