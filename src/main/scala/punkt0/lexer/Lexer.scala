package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  var lastSeenToken : Option[Token] = None
  var previousIsWhitespace = false
  var nextChar : Char = ' '

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    // TODO: implement this method

    var currentChar = source.next()

    new Iterator[Token] {

      def hasNext = {
        ???
      }

      def next = {

        //TODO fix EOF

        nextChar = source.next()

        var res = a(currentChar,source)

        currentChar = nextChar
        res

      }
    }
  }
  /*def validStart() : Boolean = lastSeenToken match {
      case Some(t) => previousIsWhitespace || !(t.isInstanceOf[ID] || t.isInstanceOf[INTLIT] || t.isInstanceOf[STRLIT])
      case None => true
    }*/
  def getToken(literal: String) : Token = literal match {
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
  //TODO better name needed
  def a (currentChar: Char, source: scala.io.BufferedSource) : Token =  {
    currentChar match {
      case digit if digit.isDigit && digit != '0' =>

        var integer = digit.asDigit
        while (nextChar.isDigit){
          integer = integer*10+nextChar.asDigit
          nextChar = source.next()
        }

        new INTLIT(integer)

      case alfa if alfa.isLetter  =>
        val buffer = new StringBuilder
        buffer.append(alfa)

        while(nextChar.isLetter || nextChar.isDigit || nextChar == '_'){
          buffer.append(nextChar)
          nextChar = source.next()
        }

        getToken(buffer.toString())

      case whiteSpace if(whiteSpace.isWhitespace) => ??? //TODO White space

      case '"' =>
        var buffer = new StringBuilder
        while(nextChar != '"'){
          buffer.append(nextChar)
          if(!source.hasNext)
            new Token(BAD) //TODO fix later
          nextChar = source.next()
        }
        new STRLIT(buffer.toString())

      case ':' => new Token(COLON)
      case ';' => new Token(SEMICOLON)
      case '.' => new Token(DOT)
      case ',' => new Token(COMMA)
      case '=' =>
        if(nextChar == '=')
          new Token(EQUALS)
        else
          new Token(EQSIGN)

      case '!' => new Token(BANG)
      case '(' => new Token(LPAREN)
      case ')' => new Token(RPAREN)
      case '{' => new Token(LBRACE)
      case '}' => new Token(RBRACE)
      case '&' =>
        if(nextChar == '&')
          new Token(AND)
        else
          new Token(BAD) //TODO fix later

      case '|' =>
        if(nextChar == '|')
          new Token(OR)
        else
          new Token(BAD) //TODO fix later

      case '<' => new Token(LESSTHAN)
      case '+' => new Token(PLUS)
      case '-' => new Token(MINUS)
      case '*' => new Token(TIMES)
      case '/' => ??? //TODO // '/' '//' '/*'
      case _   => new Token(BAD) //TODO fix consume char or have multiple bads until valid char
    }
  }
}
