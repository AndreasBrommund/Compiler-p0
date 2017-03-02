package punkt0
package ast

import Trees._
import lexer._

import scala.collection.mutable.ListBuffer

object Parser extends Phase[Iterator[Token], Program] {
  def run(tokens: Iterator[Token])(ctx: Context): Program = {
    import Reporter._
    /** Store the current token, as read from the lexer. */
    var currentToken: Token = new Token(BAD)

    def readToken: Unit = {
      if (tokens.hasNext) {
        // uses nextToken from the Lexer trait
        currentToken = tokens.next

        // skips bad tokens
        while (currentToken.kind == BAD) {
          currentToken = tokens.next
        }
      }
    }

    /** ''Eats'' the expected token, or terminates with an error. */
    def eat(kind: TokenKind): Unit = {
      if (currentToken.kind == kind) {
        readToken
      } else {
        expected(kind)
      }
    }

    /** Complains that what was found was not expected. The method accepts arbitrarily many arguments of type TokenKind */
    def expected(kind: TokenKind, more: TokenKind*): Nothing = {
      fatal("expected: " + (kind :: more.toList).mkString(" or ") + ", found: " + currentToken, currentToken)
    }

    //Program ::=
    def parseGoal: Program = {
      var classes = new ListBuffer[ClassDecl]
      while (currentToken.kind == CLASS) {
        classes += parseClassDeclaration
      }
      new Program(parseMainDeclaration, classes.toList)
    }

    //ClassDeclaration ::=
    def parseClassDeclaration: ClassDecl = {
      ???
    }

    //MainDeclaration ::=
    def parseMainDeclaration: MainMethod = {
      eat(OBJECT)
      val obj = parseIdent
      eat(EXTENDS)
      val parent = parseIdent
      eat(LBRACE)

      val varDecls = new ListBuffer[VarDecl]
      //TODO Add decls to the list
      var exprs = new ListBuffer[ExprTree]
      //TODO Add exprs to the list


      val main = new MethodDecl(
        false, new UnitType, new Identifier("Main"), List(),
        varDecls.toList, exprs.init.toList, exprs.last)
      eat(RBRACE)
      new MainMethod(obj, parent, main)
    }

    //VarDeclaration ::=
    def parseVarDeclaration: VarDecl = {
      ???
    }

    //Type ::=
    def parseType: TypeTree = {
      //TODO maybe read token
      currentToken.kind match {
        case BOOLEAN => new BooleanType
        case INT => new IntType
        case STRING => new StringType
        case UNIT => new UnitType
        case _ => parseIdent
      }
    }

    //Identifier ::=
    def parseIdent: Identifier = currentToken match {
      case id: ID => new Identifier(id.value)
      case _ => expected(IDKIND)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
