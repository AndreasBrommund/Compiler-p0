package punkt0
package analyzer

import ast.Trees._
import Symbols._
import Reporter._

object NameAnalysis extends Phase[Program, Program] {

  def run(prog: Program)(ctx: Context): Program = {


    val globalScope = new GlobalScope
    joinProgramSymbols(prog,globalScope)


    //TODO handle setSymbols




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
      //TODO check if the class name is used
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
          for (param <- method.args){
            methodSymbol.lookupVar(param.id.value) match {
              case Some(sym) => Reporter.error("Parameter '"+param.id.value+"' is already declared at position: "+sym.posString,param)
              case None =>
                val variableSymbol = new VariableSymbol(param.id.value).setPos(param)
                methodSymbol.params += (variableSymbol.name -> variableSymbol)
                methodSymbol.argList = methodSymbol.argList :+ variableSymbol

                param.setSymbol(variableSymbol)

                //TODO Add variables
            }
          }
          classSymbol.methods += (methodSymbol.name -> methodSymbol)

      }
    }
  }



}
