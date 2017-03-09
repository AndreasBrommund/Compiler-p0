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
      val exprTree = parseExpressionOr
      readToken
      exprTree
    }


    //Expression or
    def parseExpressionOr: ExprTree = {
      val andExpr = parseExpressionAnd
      if(currentToken.kind == OR){
        eat(OR)
        new Or(andExpr,parseExpressionAnd)
      }else{
        andExpr
      }
    }

    //Expression and
    def parseExpressionAnd: ExprTree = {
      val compExpr = parseExpressionCompare
      if(currentToken.kind == AND){
        eat(AND)
        new And(compExpr,parseExpressionCompare)
      }else{
        compExpr
      }
    }

    //Expression compare
    def parseExpressionCompare: ExprTree = {
      val termExpr = parseExpressionTerm
      if(currentToken.kind == LESSTHAN){
        eat(LESSTHAN)
        new LessThan(termExpr,parseExpressionTerm)
      }else if(currentToken.kind == EQUALS){
        eat(EQUALS)
        new Equals(termExpr,parseExpressionTerm)
      }else {
        termExpr
      }
    }

    //Expression term
    def parseExpressionTerm: ExprTree = {
      val factorExpr = parseExpressionFactor
      if(currentToken.kind == PLUS){
        eat(PLUS)
        new Plus(factorExpr,parseExpressionFactor)
      }else if(currentToken.kind == MINUS){
        eat(MINUS)
        new Minus(factorExpr,parseExpressionFactor)
      }else{
        factorExpr
      }
    }

    //Expression factor
    def parseExpressionFactor: ExprTree = {
      val notExpr = parseExpressionNot
      if(currentToken.kind == TIMES){
        eat(TIMES)
        new Times(notExpr,parseExpressionNot)
      }else if(currentToken.kind == DIV){
        eat(DIV)
        new Div(notExpr,parseExpressionNot)
      }else{
        notExpr
      }
    }

    //Expression not
    def parseExpressionNot: ExprTree = {
      if(currentToken.kind == BANG){
        eat(BANG)
        new Not(parseExpressionOverall)
      }else{
        parseExpressionOverall
      }
    }

    def parseExpressionOverall : ExprTree = {
      currentToken match {
        case id: INTLIT =>
          readToken
          new IntLit(id.value)
        case id: STRLIT =>
          readToken
          new StringLit(id.value)
        case id  =>
          id.kind match {
            case TRUE  =>
              eat(TRUE)
              new True
            case FALSE =>
              eat(FALSE)
              new False
            case IDKIND =>
              val ident = parseIdent
              if(currentToken.kind == EQSIGN){
                eat(EQSIGN)
                new Assign(ident,parseExpression)
              }else{
                ident
              }
            case THIS =>
              eat(THIS)
              new This
            case NULL =>
              eat(NULL)
              new Null
            case NEW =>
              eat(NEW)
              val ret = new New(parseIdent)
              eat(LPAREN)
              eat(RPAREN)
              ret;
            case LPAREN =>
              eat(LPAREN)
              val ret = parseExpression
              eat(RPAREN)
              ret
            case LBRACE =>
              eat(LPAREN)
              if(currentToken.kind != RBRACE){
                val ret = new Block(parseExpressionBlock)
                eat(RBRACE)
                ret
              }else{
                eat(RBRACE)
                new Block(List())
              }
            case IF =>
              eat(LPAREN)
              val condition = parseExpression
              eat(RPAREN)
              val ifBody = parseExpression
              var elseBody: Option[ExprTree] = None

              if(currentToken.kind == ELSE){
                eat(ELSE)
                elseBody = Some(parseExpression)
              }

              new If(condition,ifBody,elseBody)
            case WHILE =>
              eat(WHILE)
              eat(LPAREN)
              val condition = parseExpression
              eat(RPAREN)
              val body = parseExpression
              new While(condition,body)
            case PRINTLN =>
              eat(PRINTLN)
              eat(LPAREN)
              val expr = parseExpression
              eat(RPAREN)
              new Println(expr)
            case _ =>
              val obj = parseExpression
              eat(DOT)
              val method = parseIdent
              eat(LPAREN)

              var args = new ListBuffer[ExprTree]
              args += parseExpression
              while(currentToken.kind == COLON){
                eat(COLON)
                args += parseExpression
              }
              eat(RPAREN)
              new MethodCall(obj,method,args.toList)
          }
      }
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
