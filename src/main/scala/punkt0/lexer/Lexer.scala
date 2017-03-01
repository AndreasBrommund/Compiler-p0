package punkt0
package lexer

import java.io.File

import punkt0.{lexer, Positioned}


object Lexer extends Phase[File, Iterator[Token]] {



  import Reporter._

  var previousIsBad = false;

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    var currentChar: Option[Char] = None
    if(source.hasNext){
      currentChar = getNextChar(source)
    }

    new Iterator[Token] {
      var hasNext = true

      def next = {
        val tuple = findToken(currentChar, f, source)
        currentChar = tuple._2
        hasNext = tuple._3

        tuple._1.kind match {
          case BAD => previousIsBad = true
          case _   => previousIsBad = false
        }
        tuple._1
      }
    }
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
              val res = handleBad("Need to end string literal",pos,nextChar, f, source)
              nextChar = res._2
              hasNext = res._3
              pos = res._4
              res._1 //TODO Hadar
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
                  val res = findToken(getNextChar(source), f, source)
                  nextChar = res._2
                  hasNext = res._3
                  pos = res._4
                  res._1
                case (Some(next) , None) =>
                  val res = handleBad("Need to end comment",pos,Some(next), f, source)
                  nextChar = res._2
                  hasNext = res._3
                  pos = res._4
                  res._1 //TODO Hadar
                case (None, None) =>
                  val res = handleBad("Need to end comment",pos,None, f, source)
                  nextChar = res._2
                  hasNext = res._3
                  pos = res._4
                  res._1 //TODO Hadar
                case _ => ??? //Not gonna happen
              }

            case Some(_) | None => new Token(DIV)
          }
        case '&' =>
          nextChar match {
            case Some('&') =>
              nextChar = getNextChar(source)
              new Token(AND)

            case next =>
              val res = handleBad("Expect &",pos,next, f, source)
              nextChar = res._2
              hasNext = res._3
              pos = res._4
              res._1 //TODO Hadar
          }
        case '|' =>
          nextChar match {
            case Some('|') =>
              nextChar = getNextChar(source)
              new Token(OR)

            case next =>
              val res = handleBad("Expect |",pos,next, f, source)
              nextChar = res._2
              hasNext = res._3
              pos = res._4
              res._1 //TODO Hadar
          }
        case '=' =>
          nextChar match {
            case Some('=') =>
              nextChar = getNextChar(source)
              new Token(EQUALS)

            case _ => new Token(EQSIGN)
          }
        case _ =>
          val res = handleBad("Invalid char",pos,nextChar, f, source)
          nextChar = res._2
          hasNext = res._3
          pos = res._4
          res._1 //TODO Hadar
      }
      (newToken.setPos(pos), nextChar,hasNext,pos)
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

  def handleBad(msg: String,pos: Positioned,next: Option[Char], f: File, source: scala.io.BufferedSource) : (Token, Option[Char],Boolean,Positioned) = {
    if(previousIsBad){
      findToken(next,f,source)
    }else{
      Reporter.error(msg,pos)
      (new Token(BAD),next,true,pos)
    }

  }

  def getNextChar(source: scala.io.BufferedSource): Option[Char] = {
    if (source.hasNext)
      Some(source.next())
    else
      None
  }
}