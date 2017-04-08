package punkt0
package analyzer

import ast.Trees._
import Symbols._
import scala.collection.mutable

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {

    linkClasses(prog.main,prog.classes,new GlobalScope)
    linkInheritance(prog.classes)
    checkCircularInheritance(prog.classes)
    Reporter.terminateIfErrors()
    linkClassVariables(prog.main.vars,prog.main.getSymbol)

    for(c <- prog.classes)
      linkClassVariables(c.vars,c.getSymbol)

    for(c <- prog.classes)
      if(c.getSymbol.parent.isDefined)
      checkClassVariables(c.vars,c.getSymbol.parent.get)

    //joinProgramSymbols(prog,globalScope)

    //TODO First look for error in extentions

    //traverseClassExtensions(prog.classes,globalScope)


    //traverseProgram(prog)


    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

  def linkClasses(main: MainDecl, classes: List[ClassDecl], globalScope: GlobalScope): Unit = {
    val mainClassSymbol = new ClassSymbol(main.obj.value).setPos(main)
    main.setSymbol(mainClassSymbol)
    globalScope.mainClass = mainClassSymbol

    for(c <- classes){
      globalScope.lookupClass(c.id.value) match {
        case Some(sym) => Reporter.error("Class '"+c.id.value+"' is already declared at position: "+sym.posString,c)
        case None =>
          val classSymbol = new ClassSymbol(c.id.value).setPos(c)
          globalScope.classes += (classSymbol.name -> classSymbol)
          c.setSymbol(classSymbol)
      }
    }
  }

  def linkInheritance(classes: List[ClassDecl]){

    val classMap = mutable.HashMap.empty[Identifier,ClassDecl]
    for(c <- classes){
      classMap += (c.id -> c)
    }

    for(cls <- classes) {
      if(cls.parent.isDefined){
        classMap.get(cls.parent.get) match {
          case Some(s) => cls.getSymbol.parent = Some(s.getSymbol)
          case None => Reporter.error("Can't extend from undefined class '" + cls.parent.get.value + "' ", cls.parent.get)
        }
      }
    }
  }

  def checkCircularInheritance(classes: List[ClassDecl]): Unit = {
    for (cls <- classes){
      if(hasCircle(cls.getSymbol,Set()))
          Reporter.error("Found circular inheritance ",cls)

    }
  }

  def hasCircle(curr: ClassSymbol,visited: Set[ClassSymbol]): Boolean ={

    lazy val res = curr.parent match {
      case None => false
      case Some(par) =>
        hasCircle(par,visited+curr)
    }

    visited.contains(curr) || res
  }

  def linkClassVariables(vars: List[VarDecl], classSymbol: ClassSymbol): Unit ={

    for(variable <- vars){
      classSymbol.members.get(variable.id.value) match {
        case Some(v) => Reporter.error("Variable '" + variable.id.value + "' is already declared at position: " + v.posString, variable)
        case None =>
          val varSymbol = new VariableSymbol(variable.id.value).setPos(variable)
          classSymbol.members += (varSymbol.name -> varSymbol)
          variable.setSymbol (varSymbol)
      }
    }
  }

  def checkClassVariables(vars: List[VarDecl], classSymbol: ClassSymbol): Unit ={
    for(variable <- vars) {
      classSymbol.lookupVar(variable.id.value) match {
        case Some(v) => Reporter.error("Variable '" + variable.id.value + "' is already declared at position: " + v.posString, variable)
        case None =>
      }
    }
  }



  


  def joinClassMethodsSymbols(methodDecl: List[MethodDecl],classSymbol: ClassSymbol) : Unit = {

    for(method <- methodDecl){
      classSymbol.lookupMethod(method.id.value) match {
        case Some(sym) => Reporter.error("Method '"+method.id.value+"' is already declared at position: "+sym.posString,method)
        case None =>
          val methodSymbol = new MethodSymbol(method.id.value,classSymbol).setPos(method)
          method.setSymbol(methodSymbol)

          //Add all parameters for a method
          for (param <- method.args){
            methodSymbol.lookupVar(param.id.value) match {
              case Some(sym) => Reporter.error("Parameter '"+param.id.value+"' is already declared at position: "+sym.posString,param)
              case None =>
                val variableSymbol = new VariableSymbol(param.id.value).setPos(param)
                methodSymbol.params += (variableSymbol.name -> variableSymbol)
                methodSymbol.argList = methodSymbol.argList :+ variableSymbol

                param.setSymbol(variableSymbol)
            }
          }

          //Add all method variables
          for (variable <- method.vars){
            methodSymbol.lookupVar(variable.id.value) match {
              case Some(sym) => Reporter.error("Variable '"+variable.id.value+"' is already declared at position: "+sym.posString,variable)
              case None =>
                val variableSymbol = new VariableSymbol(variable.id.value).setPos(variable)
                methodSymbol.members += (variableSymbol.name -> variableSymbol)

                variable.setSymbol(variableSymbol)
            }
          }

          classSymbol.methods += (methodSymbol.name -> methodSymbol)

      }
    }
  }





  def traverseIdentifiers(branch: Tree) : Unit = {
    branch match {
      case node: Program =>

      case node: MainDecl =>
        traverseIdentifiers(node.obj)
        traverseIdentifiers(node.parent)
        node.vars.foreach(traverseIdentifiers)
        node.exprs.foreach(traverseIdentifiers)
      case node: ClassDecl =>
        traverseIdentifiers(node.id)
        if(node.parent.isDefined){
          //TODO check if parent exist
          traverseIdentifiers(node.parent.get)
        }
        node.vars.foreach(traverseIdentifiers)
        node.methods.foreach(traverseIdentifiers)
      case node: VarDecl =>
        traverseIdentifiers(node.id)
        traverseIdentifiers(node.tpe)
        traverseIdentifiers(node.expr)
      case node: MethodDecl =>
        if(node.overrides){
          //TODO check if the overiding method exist
        }
        traverseIdentifiers(node.id)
        if (node.args.nonEmpty) {
          node.args.foreach(traverseIdentifiers)
        }
        traverseIdentifiers(node.retType)
        node.vars.foreach(traverseIdentifiers)
        if(node.exprs.nonEmpty){
          node.exprs.foreach(traverseIdentifiers)

        }
        traverseIdentifiers(node.retExpr)
      case node: Formal =>
        traverseIdentifiers(node.id)
        traverseIdentifiers(node.tpe)
      case node: And =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: Or =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: Plus =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: Minus =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: Times =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: Div =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: LessThan =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: Equals =>
        traverseIdentifiers(node.lhs)
        traverseIdentifiers(node.rhs)
      case node: MethodCall =>
        //TODO Handle differnet
        traverseIdentifiers(node.obj)
        traverseIdentifiers(node.meth)
        if (node.args.nonEmpty) {
          node.args.foreach(traverseIdentifiers)
        }
      case node: Identifier => //TODO Handle
      case node: New =>
        traverseIdentifiers(node.tpe)
      case node: Not =>
        traverseIdentifiers(node.expr)
      case node: Block =>
        if (node.exprs.nonEmpty) {
          node.exprs.foreach(traverseIdentifiers)
        }
      case node: If =>
        traverseIdentifiers(node.expr)
        traverseIdentifiers(node.thn)
        if(node.els.isDefined){
          traverseIdentifiers(node.els.get)
        }
      case node: While =>
        traverseIdentifiers(node.cond)
        traverseIdentifiers(node.body)
      case node: Println =>
        traverseIdentifiers(node.expr)
      case node: Assign =>
        traverseIdentifiers(node.id)
        traverseIdentifiers(node.expr)
      case _ =>
    }
  }

}
