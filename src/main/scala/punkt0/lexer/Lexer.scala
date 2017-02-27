package punkt0
package lexer

import java.io.File

import punkt0.Positioned


object Lexer extends Phase[File, Iterator[Token]] {



  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    var currentChar: Option[Char] = Some(source.next()) //TODO breaks if the file is empty


    new Iterator[Token] {
      var hasNext = true

      def next = {
        val tuple = findToken(currentChar, f, source)
        currentChar = tuple._2
        hasNext = tuple._3
        tuple._1
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
  def findToken(curr: Option[Char], f: File, source: scala.io.BufferedSource): (Token, Option[Char],Boolean,Positioned) = {
    var currentChar = curr
    var hasNext = true

    while (currentChar.isDefined && currentChar.get.isWhitespace) {
      currentChar = getNextChar(source)
    }

    var pos = new Positioned {
      setPos(f, source.pos)
    }

    if(currentChar.isEmpty) {
      (new Token(EOF).setPos(pos),currentChar,false,pos)
    }else {
      var nextChar = getNextChar(source)

      val newToken = currentChar.get match {
        case digit if digit.isDigit && digit != '0' =>
          var integer = digit.asDigit

          while (nextChar.isDefined && nextChar.get.isDigit) {
            integer = integer * 10 + nextChar.get.asDigit
            nextChar = getNextChar(source)
          }

          new INTLIT(integer)

        case alfa if alfa.isLetter =>
          val buffer = new StringBuilder
          buffer.append(alfa)

          while (nextChar.isDefined && (nextChar.get.isLetterOrDigit || nextChar.get == '_')) {
            buffer.append(nextChar.get)
            nextChar = getNextChar(source)
          }

          getToken(buffer.toString)

        case '"' =>
          val buffer = new StringBuilder

          while (nextChar.isDefined && nextChar.get != '\n' && nextChar.get != '"') {
            buffer.append(nextChar.get)
            nextChar = getNextChar(source)
          }

          nextChar match {
            case Some('"') =>
              nextChar = getNextChar(source)
              new STRLIT(buffer.toString)
            case _ => //Always gonna be None or \n
              nextChar = getNextChar(source)
              new Token(BAD) //TODO handle BAD better

          }

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
        case '/' =>
          nextChar match {
            case Some('/') =>
              nextChar = getNextChar(source)
              while (nextChar.isDefined && nextChar.get != '\n') {
                nextChar = getNextChar(source)
              }

              nextChar match {
                case Some(c) =>
                  val r = findToken(Some(c), f, source)
                  nextChar = r._2
                  hasNext = r._3
                  pos = r._4
                  r._1
                case None =>
                  pos = new Positioned {
                    setPos(f, source.pos)
                  }
                  hasNext = false
                  new Token(EOF)
              }
            case Some('*') =>
              nextChar = getNextChar(source)
              var secondNext = getNextChar(source)
              while (nextChar.isDefined&&secondNext.isDefined && !(nextChar.get == '*' && secondNext.get == '/')){
                nextChar = secondNext
                secondNext = getNextChar(source)
              }
              (nextChar,secondNext) match {
                case (Some(_),Some(_)) =>
                  val r = findToken(getNextChar(source), f, source)
                  nextChar = r._2
                  hasNext = r._3
                  pos = r._4
                  r._1
                case _ =>
                  nextChar = None
                  new Token(BAD) //TODO fix later
              }

            case Some(_) | None => new Token(DIV)
          }
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
        case '=' =>
          nextChar match {
            case Some('=') =>
              nextChar = getNextChar(source)
              new Token(EQUALS)

            case _ => new Token(EQSIGN)
          }
        case _ => new Token(BAD) //TODO fix consume char or have multiple bads until valid char
      }
      (newToken.setPos(pos), nextChar,hasNext,pos)
    }
  }



  def getNextChar(source: scala.io.BufferedSource): Option[Char] = {
    if (source.hasNext)
      Some(source.next())
    else
      None
  }
}