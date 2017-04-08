package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Reporter._
import scala.collection.mutable

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {


    val globalScope = new GlobalScope
    joinProgramSymbols(prog,globalScope)

    //TODO First look for error in extentions

    traverseClassExtensions(prog.classes,globalScope)

    traverseProgram(prog)


    // Step 1: Collect symbols in declarations
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    // (Step 3:) Print tree with symbol ids for debugging

    // Make sure you check all constraints

    prog
  }

  def joinProgramSymbols(program: Program, globalScope: GlobalScope) : Unit = {

    //Handle main class
    val mainClassSymbol = new ClassSymbol(program.main.obj.value).setPos(program.main)
    joinClassVariableSymbols(program.main.vars,mainClassSymbol)

    globalScope.mainClass = mainClassSymbol

    program.main.setSymbol(mainClassSymbol)

    //Handle all classes

    for(c <- program.classes){
      globalScope.lookupClass(c.id.value) match {
        case Some(sym) => Reporter.error("Class '"+c.id.value+"' is already declared at position: "+sym.posString,c)
        case None =>

          val classSymbol = new ClassSymbol(c.id.value).setPos(c)
          joinClassVariableSymbols(c.vars,classSymbol)

          joinClassMethodsSymbols(c.methods,classSymbol)

          globalScope.classes += (classSymbol.name -> classSymbol)

          c.setSymbol(classSymbol)
      }
    }
  }

  def joinClassVariableSymbols(vars: List[VarDecl],classSymbol: ClassSymbol) : Unit = {

    for(decl <- vars){
      classSymbol.lookupVar(decl.id.value) match {
        case Some(sym) => Reporter.error("Variable '"+decl.id.value+"' is already declared at position: "+sym.posString,decl)
        case None =>
          val varSymbol = new VariableSymbol(decl.id.value).setPos(decl)
          classSymbol.members += (varSymbol.name -> varSymbol)
          decl.setSymbol(varSymbol)
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

  def traverseClassExtensions(classes: List[ClassDecl], globalScope: GlobalScope) : Unit = {

    val visited = mutable.HashMap.empty[ClassDecl,Boolean]
    val classMap = mutable.HashMap.empty[Identifier,ClassDecl]

    for(c <- classes){
      classMap += (c.id -> c)
    }

    for(cls <- classes) {
      if (!visited(cls)) {
        var clsIt = cls
        while (clsIt.parent.isDefined) {

          globalScope.lookupClass(clsIt.parent.get.value) match{
            case None => Reporter.error("Class '"+cls.parent.get.value+"' is not defined",cls)
            case Some(parent) =>
              visited += (clsIt -> true)
              clsIt.getSymbol.parent = Some(parent)
              clsIt = classMap(clsIt.parent.get)

          }

          if(clsIt.equals(cls)){
            Reporter.error("Inheritance graph has a cycle",cls)
          }
        }
        visited += (cls -> true)
      }
    }
  }


  def traverseProgram(program: Program) : Unit = {
    program.classes.foreach(traverseClass)
    traverseIdentifiers(program.main)
  }

  def traverseClass(c: ClassDecl) : Unit = {
    //TODO a
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
