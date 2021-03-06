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
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var classes = new ListBuffer[ClassDecl]
      while (currentToken.kind == CLASS) {
        classes += parseClassDeclaration
      }
      val mainDecl = parseMainDeclaration
      eat(EOF)
      new Program(mainDecl, classes.toList).setPos(firstTokenPos)
    }

    //ClassDeclaration ::=
    def parseClassDeclaration: ClassDecl = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      eat(CLASS)
      val className = parseIdent

      //Extends
      var extendsName: Option[Identifier] = None
      if (currentToken.kind == EXTENDS) {
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
      new ClassDecl(className, extendsName, vars.toList, methods.toList).setPos(firstTokenPos)
    }

    //MainDeclaration ::=
    def parseMainDeclaration: MainDecl = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      eat(OBJECT)
      val obj = parseIdent
      eat(EXTENDS)
      val parent = parseIdent
      eat(LBRACE)

      val vars = parseVarDeclarationBlock

      val exprs = parseExpressionBlock

      eat(RBRACE)
      new MainDecl(obj, parent, vars, exprs).setPos(firstTokenPos)
    }

    //VarDeclaration ::=
    def parseVarDeclaration: VarDecl = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      eat(VAR)
      val varName = parseIdent
      eat(COLON)
      val varType = parseType
      eat(EQSIGN)

      //Takes only constant exprs
      val varExpr = currentToken match {
        case integer: INTLIT =>
          readToken
          val i = integer.value
          new IntLit(i).setPos(integer)
        case string: STRLIT =>
          readToken
          val s = string.value
          new StringLit(s).setPos(string)
        case tok => currentToken.kind match {
          case TRUE =>
            eat(TRUE)
            new True().setPos(tok)
          case FALSE =>
            eat(FALSE)
            new False().setPos(tok)
          case NULL =>
            eat(NULL)
            new Null().setPos(tok)
          case NEW =>
            eat(NEW)
            val id = parseIdent
            eat(LPAREN)
            eat(RPAREN)
            new New(id).setPos(tok)
          case _ => expected(INTLITKIND,STRLITKIND,TRUE,FALSE,NULL,NEW)
        }
      }


      eat(SEMICOLON)
      new VarDecl(varType, varName, varExpr).setPos(firstTokenPos)
    }

    def parseVarDeclarationBlock: List[VarDecl] = {
      var vars = new ListBuffer[VarDecl]
      while (currentToken.kind == VAR)
        vars += parseVarDeclaration
      vars.toList
    }

    //MethodDeclaration ::=
    def parseMethodDeclaration: MethodDecl = {
      var overrides = false
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      currentToken.kind match {
        case OVERRIDE => overrides = true; readToken; eat(DEF)
        case DEF => eat(DEF)
        case _ => expected(OVERRIDE, DEF)
      }


      val methodName = parseIdent
      eat(LPAREN)

      var arguments = new ListBuffer[Formal]

      //Get all method arguments
      if (currentToken.kind == IDKIND) {
        //Have arguments

        def parseFormal = {
          val pos = currentToken.asInstanceOf[Positioned]
          val argumentName = parseIdent
          eat(COLON)
          val argumentType = parseType
          arguments += new Formal(argumentType, argumentName).setPos(pos)
        }

        parseFormal //Get the first required argument
        while (currentToken.kind == COMMA) {
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

      new MethodDecl(overrides, returnType, methodName,
        arguments.toList, vars, exprs.init, exprs.last).setPos(firstTokenPos)

    }

    //Type ::=
    def parseType: TypeTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      val tree = currentToken.kind match {
        case BOOLEAN => eat(BOOLEAN); new BooleanType
        case INT => eat(INT); new IntType
        case STRING => eat(STRING); new StringType
        case UNIT => eat(UNIT); new UnitType
        case _ => parseIdent
      }
      tree.setPos(firstTokenPos)
    }

    //Expression ::=
    def parseExpression: ExprTree = parseExpressionOr

    def parseExpressionMethodCall: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var obj = parseExpressionOverall
      while(currentToken.kind == DOT){
        eat(DOT)
        val method = parseIdent
        eat(LPAREN)

        var args = new ListBuffer[ExprTree]
        if(currentToken.kind != RPAREN) {
          args += parseExpression
          while (currentToken.kind == COMMA) {
            eat(COMMA)
            args += parseExpression
          }
        }
        eat(RPAREN)
        obj = new MethodCall(obj, method, args.toList).setPos(firstTokenPos)
      }
      obj
    }






    //Expression or
    def parseExpressionOr: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var andExpr = parseExpressionAnd
      while (currentToken.kind == OR) {
        eat(OR)
        andExpr = new Or(andExpr, parseExpressionAnd).setPos(firstTokenPos)
      }
      andExpr
    }

    //Expression and
    def parseExpressionAnd: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var compExpr = parseExpressionCompare
      while (currentToken.kind == AND) {
        eat(AND)
        compExpr = new And(compExpr, parseExpressionCompare).setPos(firstTokenPos)
      }
      compExpr
    }


    //Expression compare
    def parseExpressionCompare: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var termExpr = parseExpressionTerm
      while (currentToken.kind == LESSTHAN || currentToken.kind == EQUALS) {
        if (currentToken.kind == LESSTHAN) {
          eat(LESSTHAN)
          termExpr = new LessThan(termExpr, parseExpressionTerm).setPos(firstTokenPos)
        } else {
          //currentToken.kind == EQUALS
          eat(EQUALS)
          termExpr = new Equals(termExpr, parseExpressionTerm).setPos(firstTokenPos)
        }
      }
      termExpr
    }

    //Expression term
    def parseExpressionTerm: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var factorExpr = parseExpressionFactor
      while (currentToken.kind == PLUS || currentToken.kind == MINUS) {
        if (currentToken.kind == PLUS) {
          eat(PLUS)
          factorExpr = new Plus(factorExpr, parseExpressionFactor).setPos(firstTokenPos)
        } else {
          //currentToken.kind == MINUS
          eat(MINUS)
          factorExpr = new Minus(factorExpr, parseExpressionFactor).setPos(firstTokenPos)
        }
      }
      factorExpr
    }

    //Expression factor
    def parseExpressionFactor: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      var notExpr = parseExpressionNot
      while (currentToken.kind == TIMES || currentToken.kind == DIV) {
        if (currentToken.kind == TIMES) {
          eat(TIMES)
          notExpr = new Times(notExpr, parseExpressionNot).setPos(firstTokenPos)
        } else {
          //currentToken.kind == DIV
          eat(DIV)
          notExpr = new Div(notExpr, parseExpressionNot).setPos(firstTokenPos)
        }
      }
      notExpr

    }

    //Expression not
    def parseExpressionNot: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      if (currentToken.kind == BANG) {
        eat(BANG)
        new Not(parseExpressionNot).setPos(firstTokenPos)
      } else {
        parseExpressionMethodCall
      }
    }

    def parseExpressionOverall: ExprTree = {
      val firstTokenPos = currentToken.asInstanceOf[Positioned]
      currentToken match {
        case id: INTLIT =>
          readToken
          new IntLit(id.value).setPos(firstTokenPos)
        case id: STRLIT =>
          readToken
          new StringLit(id.value).setPos(firstTokenPos)
        case id =>
          id.kind match {
            case TRUE =>
              eat(TRUE)
              new True().setPos(firstTokenPos)
            case FALSE =>
              eat(FALSE)
              new False().setPos(firstTokenPos)
            case IDKIND =>
              val ident = parseIdent
              if (currentToken.kind == EQSIGN) {
                eat(EQSIGN)
                new Assign(ident, parseExpression).setPos(firstTokenPos)
              } else {
                ident
              }
            case THIS =>
              eat(THIS)
              new This().setPos(firstTokenPos)
            case NULL =>
              eat(NULL)
              new Null().setPos(firstTokenPos)
            case NEW =>
              eat(NEW)
              val ret = new New(parseIdent).setPos(firstTokenPos)
              eat(LPAREN)
              eat(RPAREN)
              ret;
            case LPAREN =>
              eat(LPAREN)
              val ret = parseExpression
              eat(RPAREN)
              ret
            case LBRACE =>
              eat(LBRACE)
              if (currentToken.kind != RBRACE) {
                val ret = new Block(parseExpressionBlock).setPos(firstTokenPos)
                eat(RBRACE)
                ret
              } else {
                eat(RBRACE)
                new Block(List()).setPos(firstTokenPos)
              }
            case IF =>
              eat(IF)
              eat(LPAREN)
              val condition = parseExpression
              eat(RPAREN)
              val ifBody = parseExpression
              var elseBody: Option[ExprTree] = None

              if (currentToken.kind == ELSE) {
                eat(ELSE)
                elseBody = Some(parseExpression)
              }

              new If(condition, ifBody, elseBody).setPos(firstTokenPos)
            case WHILE =>
              eat(WHILE)
              eat(LPAREN)
              val condition = parseExpression
              eat(RPAREN)
              val body = parseExpression
              new While(condition, body).setPos(firstTokenPos)
            case PRINTLN =>
              eat(PRINTLN)
              eat(LPAREN)
              val expr = parseExpression
              eat(RPAREN)
              new Println(expr).setPos(firstTokenPos)
            case _ => fatal("expected: expression, found: " + currentToken, currentToken)
          }
      }
    }

    def parseExpressionBlock: List[ExprTree] = {
      var exprs = new ListBuffer[ExprTree]
      exprs += parseExpression
      while (currentToken.kind == SEMICOLON) {
        eat(SEMICOLON)
        exprs += parseExpression
      }
      exprs.toList
    }

    //Identifier ::=
    def parseIdent: Identifier = currentToken match {
      case id: ID =>
        val firstTokenPos = currentToken.asInstanceOf[Positioned]
        readToken
        new Identifier(id.value).setPos(firstTokenPos)
      case _ => expected(IDKIND)
    }

    readToken
    val tree = parseGoal
    terminateIfErrors
    tree
  }
}
