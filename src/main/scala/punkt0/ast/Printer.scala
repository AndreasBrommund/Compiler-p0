package punkt0
package ast

import Trees._
import analyzer.Symbols._

object Printer {

  var doSymId = false

  def apply(t: Tree, doSymId: Boolean): String = {
    val sb = new StringBuilder
    this.doSymId = doSymId
    getPrettyString(t,sb)
    sb.toString
  }

  def getPrettyString(branch: Tree, sb: StringBuilder) : Unit = {
    branch match {
      case node: Program =>
        node.classes.foreach(n => getPrettyString(n, sb))
        getPrettyString(node.main, sb)
      case node: MainDecl =>
        sb.append(" object ")
        getPrettyString(node.obj, sb)
        addId(node.getSymbol,sb)
        sb.append(" extends ")
        getPrettyString(node.parent, sb)
        sb.append(" {\n")
        node.vars.foreach(n => getPrettyString(n, sb))
        getPrettyString(node.exprs.head, sb)
        node.exprs.tail.foreach(n => {
          sb.append(";\n"); getPrettyString(n, sb)
        })
        sb.append("\n}\n")
      case node: ClassDecl =>
        sb.append(" class ")
        getPrettyString(node.id,sb)
        addId(node.getSymbol,sb)
        if(node.parent.isDefined){
          sb.append(" extends ")
          getPrettyString(node.parent.get,sb)
          addId(node.getSymbol.parent.get,sb)
        }
        sb.append(" {\n")
        node.vars.foreach(n => getPrettyString(n, sb))
        node.methods.foreach(n => {
          sb.append("\n"); getPrettyString(n, sb)
        })
        sb.append("\n}\n")
      case node: VarDecl =>
        sb.append(" var ")
        getPrettyString(node.id, sb)
        addId(node.getSymbol,sb)
        sb.append(" : ")
        getPrettyString(node.tpe, sb)
        sb.append("=")
        getPrettyString(node.expr, sb)
        sb.append(";\n")
      case node: MethodDecl =>
        if(node.overrides){
          sb.append(" override ")
        }
        sb.append(" def ")
        getPrettyString(node.id,sb)
        addId(node.getSymbol,sb)
        sb.append(" (")
        if (node.args.nonEmpty) {
          getPrettyString(node.args.head, sb)
          node.args.tail.foreach(n => {sb.append(","); getPrettyString(n, sb)})
        }
        sb.append("): ")
        getPrettyString(node.retType,sb)
        sb.append(" = {\n")
        node.vars.foreach(n => getPrettyString(n, sb))
        if(node.exprs.nonEmpty){
          getPrettyString(node.exprs.head, sb)
          node.exprs.tail.foreach(n => {sb.append(";\n"); getPrettyString(n, sb)})
          sb.append(";\n")
        }
        getPrettyString(node.retExpr,sb)
        sb.append("\n}\n")
      case node: Formal =>
        getPrettyString(node.id,sb)
        addId(node.getSymbol,sb)
        sb.append(" : ")
        getPrettyString(node.tpe,sb)
      case node: BooleanType => sb.append(" Boolean ")
      case node: IntType => sb.append(" Int ")
      case node: StringType => sb.append(" String ")
      case node: UnitType => sb.append(" Unit ")
      case node: And =>
        sb.append("(")
        getPrettyString(node.lhs,sb)
        sb.append("&&")
        getPrettyString(node.rhs,sb)
        sb.append(")")
      case node: Or =>
        sb.append("(")
        getPrettyString(node.lhs,sb)
        sb.append("||")
        getPrettyString(node.rhs,sb)
        sb.append(")")
      case node: Plus =>
        sb.append("(")
        getPrettyString(node.lhs, sb)
        sb.append("+")
        getPrettyString(node.rhs, sb)
        sb.append(")")
      case node: Minus =>
        sb.append("(")
        getPrettyString(node.lhs, sb)
        sb.append("-")
        getPrettyString(node.rhs, sb)
        sb.append(")")
      case node: Times =>
        sb.append("(")
        getPrettyString(node.lhs, sb)
        sb.append("*")
        getPrettyString(node.rhs, sb)
        sb.append(")")
      case node: Div =>
        sb.append("(")
        getPrettyString(node.lhs, sb)
        sb.append("/")
        getPrettyString(node.rhs, sb)
        sb.append(")")
      case node: LessThan =>
        sb.append("(")
        getPrettyString(node.lhs, sb)
        sb.append("<")
        getPrettyString(node.rhs, sb)
        sb.append(")")
      case node: Equals =>
        sb.append("(")
        getPrettyString(node.lhs, sb)
        sb.append("==")
        getPrettyString(node.rhs, sb)
        sb.append(")")
      case node: MethodCall =>
        getPrettyString(node.obj,sb)
        sb.append(".")
        getPrettyString(node.meth,sb)
        if(doSymId){
          sb.append("#??")
        }
        sb.append(" (")
        if (node.args.nonEmpty) {
          getPrettyString(node.args.head, sb)
          node.args.tail.foreach(n => {sb.append(","); getPrettyString(n, sb)})
        }
        sb.append(")")
      case node: IntLit => sb.append(node.value)
      case node: StringLit =>
        sb.append("\"")
        sb.append(node.value)
        sb.append("\"")
      case node: True => sb.append(" true ")
      case node: False => sb.append(" false ")
      case node: Identifier =>
        sb.append(node.value)
        if(doSymId&&node.hasSymbol){
          sb.append("#"+node.getSymbol.id)
        }
      case node: This => sb.append(" this ")
      case node: Null => sb.append(" null ")
      case node: New =>
        sb.append(" new ")
        getPrettyString(node.tpe, sb)
        sb.append("()")
      case node: Not =>
        sb.append("!(")
        getPrettyString(node.expr, sb)
        sb.append(")")
      case node: Block =>
        sb.append("\n{\n")
        if (node.exprs.nonEmpty) {
          getPrettyString(node.exprs.head, sb)
          node.exprs.tail.foreach(n => {sb.append(";\n"); getPrettyString(n, sb)})
        }
        sb.append("\n}\n")
      case node: If =>
        sb.append(" if(")
        getPrettyString(node.expr,sb)
        sb.append(") ")
        getPrettyString(node.thn,sb)
        if(node.els.isDefined){
          sb.append(" else ")
          getPrettyString(node.els.get,sb)
        }
      case node: While =>
        sb.append(" while(")
        getPrettyString(node.cond,sb)
        sb.append(")\n")
        getPrettyString(node.body,sb)
      case node: Println =>
        sb.append(" println(")
        getPrettyString(node.expr,sb)
        sb.append(")")
      case node: Assign =>
        getPrettyString(node.id,sb)
        sb.append("=")
        getPrettyString(node.expr,sb)
      case _ => Reporter.fatal("Wrong node type in tree",branch)
    }
  }
  def addId(symbol: Symbol,sb: StringBuilder): Unit ={
    if (doSymId)
      sb.append("#"+symbol.id)

  }
}
