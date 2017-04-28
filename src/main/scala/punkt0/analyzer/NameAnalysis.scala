package punkt0
package analyzer

import ast.Trees._
import Types._
import Symbols._
import scala.collection.mutable



object NameAnalysis extends Phase[Program, Program] {

  val globalScope: GlobalScope = new GlobalScope

  def run(prog: Program)(ctx: Context): Program = {

    //Go through all declarations
    linkClasses(prog.main, prog.classes)
    linkInheritance(prog.classes)
    Reporter.terminateIfErrors()

    checkCircularInheritance(prog.classes)
    Reporter.terminateIfErrors()

    //Go through all class fields
    linkClassVariables(prog.main.vars, prog.main.getSymbol)
    for (c <- prog.classes)
      linkClassVariables(c.vars, c.getSymbol)
    Reporter.terminateIfErrors()

    for (c <- prog.classes) {
      if (c.getSymbol.parent.isDefined) {
        checkClassVariables(c.vars, c.getSymbol.parent.get)
      }
    }
    Reporter.terminateIfErrors()

    //Go through all class methods
    for (c <- prog.classes) {
      for (m <- c.methods) {
        linkClassMethod(m, c.getSymbol)
        Reporter.terminateIfErrors()
        linkMethodVariables(m.args, m.vars, m.getSymbol)
      }
    }
    Reporter.terminateIfErrors()

    for (c <- prog.classes) {
      checkClassMethod(c.methods, c.getSymbol)
    }
    Reporter.terminateIfErrors()

    //Go through the main object
    for (mainVar <- prog.main.vars) {
      linkType(mainVar.tpe)
      linkIdentExprs(mainVar.expr,prog.main.getSymbol.lookupVar)
    }
    for (mainExp <- prog.main.exprs){
      linkIdentExprs(mainExp,prog.main.getSymbol.lookupVar)
    }

    //Go through all the exprs
    for (cls <- prog.classes) {
      for (clsVar <- cls.vars) {
        linkType(clsVar.tpe)
        linkIdentExprs(clsVar.expr,cls.getSymbol.lookupVar)
      }
      for (method <- cls.methods) {
        for (param <- method.args) {
          linkType(param.tpe)
        }
        for (methodVar <- method.vars) {
          linkType(methodVar.tpe)
          linkIdentExprs(methodVar.expr,method.getSymbol.lookupVar)
        }
        linkType(method.retType)
        for (methodExp <- method.exprs){
          linkIdentExprs(methodExp,method.getSymbol.lookupVar)
        }
        linkIdentExprs(method.retExpr,method.getSymbol.lookupVar)
      }
    }



    Reporter.terminateIfErrors()


    //TODO var decl constant ??????

    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

  def linkClasses(main: MainDecl, classes: List[ClassDecl]): Unit = {
    val mainClassSymbol = new ClassSymbol(main.obj.value).setPos(main)
    mainClassSymbol.setType(new TAnyRef(mainClassSymbol))
    main.setSymbol(mainClassSymbol)
    globalScope.mainClass = mainClassSymbol

    if (main.parent.value == "App") {
      main.getSymbol.parent = Some(new ClassSymbol("App"))
    } else {
      Reporter.error("Main class doesn't extend App", main.parent)
    }

    for (c <- classes) {
      globalScope.lookupClass(c.id.value) match {
        case Some(sym) => Reporter.error("Class '" + c.id.value + "' is already declared at position: " + sym.posString, c)
        case None =>
          val classSymbol = new ClassSymbol(c.id.value).setPos(c)
          classSymbol.setType(new TAnyRef(classSymbol))
          globalScope.classes += (classSymbol.name -> classSymbol)
          c.setSymbol(classSymbol)
      }
    }
  }

  def linkInheritance(classes: List[ClassDecl]) {

    val classMap = mutable.HashMap.empty[Identifier, ClassDecl]
    for (c <- classes) {
      classMap += (c.id -> c)
    }

    for (cls <- classes) {
      if (cls.parent.isDefined) {
        classMap.get(cls.parent.get) match {
          case Some(s) => cls.getSymbol.parent = Some(s.getSymbol)
          case None => Reporter.error("Can't extend from undefined class '" + cls.parent.get.value + "' ", cls.parent.get)
        }
      }
    }
  }

  def checkCircularInheritance(classes: List[ClassDecl]): Unit = {
    for (cls <- classes) {
      if (hasCircle(cls.getSymbol, Set()))
        Reporter.error("Found circular inheritance ", cls)

    }
  }

  def hasCircle(curr: ClassSymbol, visited: Set[ClassSymbol]): Boolean = {

    lazy val res = curr.parent match {
      case None => false
      case Some(par) =>
        hasCircle(par, visited + curr)
    }

    visited.contains(curr) || res
  }

  def linkClassVariables(vars: List[VarDecl], classSymbol: ClassSymbol): Unit = {

    for (variable <- vars) {
      classSymbol.members.get(variable.id.value) match {
        case Some(v) => Reporter.error("Variable '" + variable.id.value + "' is already declared at position: " + v.posString, variable)
        case None =>
          val varSymbol = new VariableSymbol(variable.id.value).setPos(variable)
          classSymbol.members += (varSymbol.name -> varSymbol)
          varSymbol.setType(typeTree2Type(variable.tpe,globalScope))
          variable.setSymbol(varSymbol)
      }
    }
  }

  def checkClassVariables(vars: List[VarDecl], classSymbol: ClassSymbol): Unit = {
    for (variable <- vars) {
      classSymbol.lookupVar(variable.id.value) match {
        case Some(v) => Reporter.error("Variable '" + variable.id.value + "' is already declared at position: " + v.posString, variable)
        case None =>
      }
    }
  }

  def linkClassMethod(method: MethodDecl, classSymbol: ClassSymbol): Unit = {
    val methodLookup = classSymbol.methods.get(method.id.value)
    methodLookup match {
      case None =>
        val methodSymbol = new MethodSymbol(method.id.value, classSymbol).setPos(method)
        classSymbol.methods += (methodSymbol.name -> methodSymbol)

        methodSymbol.setType(typeTree2Type(method.retType,globalScope))

        method.setSymbol(methodSymbol)
      case Some(m) => Reporter.error("Method '" + method.id.value + "' is already declared at position: " + m.posString, method)

    }
  }

  def linkMethodVariables(params: List[Formal], vars: List[VarDecl], methodSymbol: MethodSymbol): Unit = {
    for (param <- params) {
      methodSymbol.params.get(param.id.value) match {
        case Some(p) => Reporter.error("Parameter '" + param.id.value + "' is already declared at position: " + p.posString, param)
        case None =>
          val variableSymbol = new VariableSymbol(param.id.value).setPos(param)
          methodSymbol.params += (variableSymbol.name -> variableSymbol)
          methodSymbol.argList = methodSymbol.argList :+ variableSymbol
          variableSymbol.setType(typeTree2Type(param.tpe,globalScope))
          param.setSymbol(variableSymbol)
      }
    }

    for (v <- vars) {

      lazy val lookupParam = methodSymbol.params.get(v.id.value)
      lazy val lookupVar = methodSymbol.members.get(v.id.value)

      (lookupParam, lookupVar) match {
        case (Some(p), _) => Reporter.error("Variable '" + v.id.value + "' can't shadow parameter at position: " + p.posString, v)
        case (_, Some(p)) => Reporter.error("Variable '" + v.id.value + "' is already declared at position: " + p.posString, v)
        case (None, None) =>
          val variableSymbol = new VariableSymbol(v.id.value).setPos(v)
          methodSymbol.members += (variableSymbol.name -> variableSymbol)
          variableSymbol.setType(typeTree2Type(v.tpe,globalScope))
          v.setSymbol(variableSymbol)
      }
    }
  }

  def checkClassMethod(methods: List[MethodDecl], classSymbol: ClassSymbol): Unit = {
    for (method <- methods) {
      if (classSymbol.parent.isDefined) {
        val parent = classSymbol.parent.get
        if (method.overrides) {
          parent.lookupMethod(method.id.value) match {
            case Some(m) =>
              if (m.argList.size == method.args.size) {
                method.getSymbol.overridden = Some(m)

                for (i  <- m.argList.indices){
                  if (m.argList(i).getType != method.getSymbol.argList(i).getType)
                    Reporter.error("Overridden method doesn't have the same method signature", method)
                }

                if (method.getSymbol.getType != m.getType)
                  Reporter.error("Overridden method doesn't have the same method signature", method)


              } else {
                Reporter.error("Overridden method doesn't have the same amount of arguments", method)
              }
            case None => Reporter.error(method.id.value + " overrides a method that doesn't exist", method)
          }
        } else {
          parent.lookupMethod(method.id.value) match {
            case Some(m) => Reporter.error("Method '" + method.id.value + "' is already declared at position: " + m.posString, method)
            case None =>
          }
        }
      } else {
        if (method.overrides) {
          Reporter.error(method.id.value + " overrides a method that doesn't exist", method)
        }
      }
    }
  }

  def linkType(typ: TypeTree): Unit = {
    typ match {
      case id@Identifier(value) =>
        globalScope.lookupClass(value) match {
          case None => Reporter.error("Type '" + value + "' is not defined: ", typ)
          case Some(cls) =>
            id.setSymbol(cls)
        }
      case _ =>
    }
  }
//ADD type to new and this  (need to know the classSymbol)
  def linkIdentExprs(expr: ExprTree, lookupVar: (String) => Option[VariableSymbol],classSymbol: ClassSymbol): Unit = {
    expr match {
      case node: And =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: Or =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: Plus =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: Minus =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: Times =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: Div =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: LessThan =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: Equals =>
        linkIdentExprs(node.lhs,lookupVar)
        linkIdentExprs(node.rhs,lookupVar)
      case node: MethodCall =>
        linkIdentExprs(node.obj,lookupVar)
        //NOT ON THIS ONE FOR NOW: linkIdentExprs(node.meth)
        node.args.foreach(a => linkIdentExprs(a,lookupVar))

      case node: Identifier =>
        lookupVar(node.value) match {
          case Some(v) => node.setSymbol(v)
          case None => Reporter.error("Identifier '" + node.value + "' is not defined: ", node)
        }
      case node: New =>
        linkType(node.tpe)
      case node: Not =>
        linkIdentExprs(node.expr,lookupVar)
      case node: Block =>
        node.exprs.foreach(exp => linkIdentExprs(exp,lookupVar))
      case node: If =>
        linkIdentExprs(node.expr,lookupVar)
        linkIdentExprs(node.thn,lookupVar)
        if (node.els.isDefined) {
          linkIdentExprs(node.els.get,lookupVar)
        }
      case node: While =>
        linkIdentExprs(node.cond,lookupVar)
        linkIdentExprs(node.body,lookupVar)
      case node: Println =>
        linkIdentExprs(node.expr,lookupVar)
      case node: Assign =>
        linkIdentExprs(node.id,lookupVar)
        linkIdentExprs(node.expr,lookupVar)
      case _ =>
    }

  }


}