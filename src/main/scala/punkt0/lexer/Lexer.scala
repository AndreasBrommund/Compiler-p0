package punkt0
package lexer

import java.io.File


object Lexer extends Phase[File, Iterator[Token]] {
  import Reporter._

  def run(f: File)(ctx: Context): Iterator[Token] = {
    val source = scala.io.Source.fromFile(f)

    // TODO: implement this method

    var current = source.next()
    var currentToken : Token

    new Iterator[Token] {

      def hasNext = {
        currentToken.kind,
      }

      def next = {
        if(current != EOF){
          new Token(EOF)
        }
        var nextChar = source.next()
        current match {
          case digit if digit.isDigit => //Digit
          case alfa if alfa.isLetter  => //Keyword or ID
          case '"' => //String literal
          case ':' => new Token(COLON)
          case ';' => new Token(SEMICOLON)
          case '.' => new Token(DOT)
          case ',' => new Token(COMMA)
          case '=' => // == or =
          case '!' => new Token(BANG)
          case '(' => new Token(LPAREN)
          case ')' => new Token(RPAREN)
          case '{' => new Token(LBRACE)
          case '}' => new Token(RBRACE)
          case '&' => //&&
          case '|' => //||
          case '<' => new Token(LESSTHAN)
          case '+' => new Token(PLUS)
          case '-' => new Token(MINUS)
          case '*' => new Token(TIMES)
          case '/' => // '/' '//' '/*'
          case _   => new Token(BAD) //consume char or have multiple bads until valid char
        }
        //Current = nextChar
        //return match funk

      }
    }
  }
}
