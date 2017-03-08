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
      val mainDecl = parseMainDeclaration
      eat(EOF)

      new Program(mainDecl,classes.toList)
    }

    //ClassDeclaration ::=
    def parseClassDeclaration: ClassDecl = {
      eat(CLASS)
      val className = parseIdent

      //Extends
      var extendsName : Option[Identifier] = None
      if(currentToken.kind == EXTENDS){
        eat(EXTENDS)
        extendsName = Some(parseIdent)
      }
      eat(LBRACE)

      //VarDeclaration
      val vars = parseVarDeclarationBlock

      //MethodDeclaration
      var methods = new ListBuffer[MethodDecl]
      while (currentToken.kind == OVERRIDE || currentToken.kind == DEF)
        methods += parseMethodDeclaration

      eat(RBRACE)
      new ClassDecl(className,extendsName,vars.toList,methods.toList)
    }

    //MainDeclaration ::=
    def parseMainDeclaration: MainDecl = {
      eat(OBJECT)
      val obj = parseIdent
      eat(EXTENDS)
      val parent = parseIdent
      eat(LBRACE)

      val vars = parseVarDeclarationBlock

      val exprs = parseExpressionBlock

      eat(RBRACE)
      new MainDecl(obj, parent, vars, exprs)
    }

    //VarDeclaration ::=
    def parseVarDeclaration: VarDecl = {
      eat(VAR)
      val varName = parseIdent
      eat(COLON)
      val varType = parseType
      eat(EQSIGN)
      val varExpr = parseExpression
      eat(SEMICOLON)
      new VarDecl(varType,varName,varExpr)
    }

    def parseVarDeclarationBlock: List[VarDecl] = {
      var vars = new ListBuffer[VarDecl]
      while(currentToken.kind == VAR)
        vars += parseVarDeclaration
      vars.toList
    }

    //MethodDeclaration ::=
    def parseMethodDeclaration: MethodDecl = {
      var overrides = false

      currentToken.kind match {
        case OVERRIDE => overrides = true; readToken; eat(DEF)
        case DEF => readToken
        case _ => expected(OVERRIDE, DEF)
      }


      val methodName = parseIdent
      eat(LPAREN)

      var arguments = new ListBuffer[Formal]

      //Get all method arguments
      if (currentToken.kind == IDKIND) {
        //Have arguments

        def parseFormal = {
          val argumentName = parseIdent
          eat(COLON)
          val argumentType = parseType
          arguments += new Formal(argumentType, argumentName)
        }

        parseFormal //Get the first required argument
        while (currentToken.kind == COMMA){
          eat(COMMA)
          parseFormal
        }
      }
      eat(RPAREN)
      eat(COLON)

      //Get return type
      val returnType = parseType

      eat(EQSIGN)
      eat(LBRACE)

      val vars = parseVarDeclarationBlock

      val exprs = parseExpressionBlock

      eat(RBRACE)

      new MethodDecl(overrides,returnType,methodName,
        arguments.toList,vars,exprs.init,exprs.last)

    }

    //Type ::=
    def parseType: TypeTree = {
      val typeTree = currentToken.kind match {
        case BOOLEAN => new BooleanType
        case INT => new IntType
        case STRING => new StringType
        case UNIT => new UnitType
        case _ => parseIdent
      }
      readToken
      typeTree
    }

    //Expression ::=
    def parseExpression: ExprTree = {
      //TODO FIX
      val exprTree = currentToken.kind match {
        case TRUE => new True
        case FALSE => new False
        case _ => expected(TRUE,FALSE) //TODO Fix
      }
      readToken
      exprTree
    }

    def parseExpressionBlock : List[ExprTree] = {
      var exprs = new ListBuffer[ExprTree]
      exprs += parseExpression
      while(currentToken.kind == SEMICOLON){
        eat(SEMICOLON)
        exprs += parseExpression
      }
      exprs.toList
    }

    //Identifier ::=
    def parseIdent: Identifier = currentToken match {
        case id: ID => readToken;new Identifier(id.value)
        case _ => expected(IDKIND)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
