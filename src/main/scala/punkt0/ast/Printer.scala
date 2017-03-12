package punkt0
package ast

import Trees._

object Printer {
  def apply(t: Tree): String = {
    val sb = new StringBuilder
    getPrettyString(t,sb)
    sb.toString
  }

  def getPrettyString(branch: Tree, sb: StringBuilder) : Unit = {
    branch match {
      case node: Program =>
        node.classes.foreach(n => getPrettyString(n, sb))
        getPrettyString(node.main, sb)
      case node: MainDecl =>
        sb.append("object ")
        getPrettyString(node.obj, sb)
        sb.append(" extends ")
        getPrettyString(node.parent, sb)
        sb.append("{")
        node.vars.foreach(n => getPrettyString(n, sb))
        getPrettyString(node.exprs.head, sb)
        node.exprs.tail.foreach(n => {
          sb.append(";"); getPrettyString(n, sb)
        })
        sb.append("}")
      case node: ClassDecl =>
        sb.append("class ")
        getPrettyString(node.id,sb)
        if(node.parent.isDefined){
          sb.append(" extends ")
          getPrettyString(node.parent.get,sb)
        }
        sb.append("{")
        node.vars.foreach(n => getPrettyString(n, sb))
        node.methods.foreach(n => {
          sb.append(" "); getPrettyString(n, sb)
        })
        sb.append("}")
      case node: VarDecl =>
        sb.append("var ")
        getPrettyString(node.id, sb)
        sb.append(":")
        getPrettyString(node.tpe, sb)
        sb.append("=")
        getPrettyString(node.expr, sb)
        sb.append(";")
      case node: MethodDecl =>
        if(node.overrides){
          sb.append("override ")
        }
        sb.append("def ")
        getPrettyString(node.id,sb)
        sb.append(" (")
        if (node.args.nonEmpty) {
          getPrettyString(node.args.head, sb)
          node.args.tail.foreach(n => {sb.append(","); getPrettyString(n, sb)})
        }
        sb.append("):")
        getPrettyString(node.retType,sb)
        sb.append(" = {")
        node.vars.foreach(n => getPrettyString(n, sb))
        if(node.exprs.nonEmpty){
          getPrettyString(node.exprs.head, sb)
          node.exprs.tail.foreach(n => {sb.append(";"); getPrettyString(n, sb)})
          sb.append(";")
        }
        getPrettyString(node.retExpr,sb)
      case node: Formal =>
        getPrettyString(node.id,sb)
        sb.append(":")
        getPrettyString(node.tpe,sb)
      case node: BooleanType => sb.append("Boolean")
      case node: IntType => sb.append("Int")
      case node: StringType => sb.append("String")
      case node: UnitType => sb.append("Unit")
      case node: And => sb.append("&&")
      case node: Or => sb.append("||")
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
        sb.append("(")
        if (node.args.nonEmpty) {
          getPrettyString(node.args.head, sb)
          node.args.tail.foreach(n => {sb.append(","); getPrettyString(n, sb)})
        }
        sb.append(")")
      case node: IntLit => sb.append(node.value)
      case node: StringLit => sb.append(node.value)
      case node: True => sb.append("true")
      case node: False => sb.append("false")
      case node: Identifier => sb.append(node.value)
      case node: This => sb.append("this")
      case node: Null => sb.append("null")
      case node: New =>
        sb.append("new ")
        getPrettyString(node.tpe, sb)
        sb.append("()")
      case node: Not =>
        sb.append("!(")
        getPrettyString(node.expr, sb)
        sb.append(")")
      case node: Block =>
        sb.append("{")
        if (node.exprs.nonEmpty) {
          getPrettyString(node.exprs.head, sb)
          node.exprs.tail.foreach(n => {sb.append(";"); getPrettyString(n, sb)})
        }
        sb.append("}")
      case node: If =>
        sb.append("if (")
        getPrettyString(node.expr,sb)
        sb.append(")")
        sb.append("{")
        getPrettyString(node.thn,sb)
        sb.append("}")
        if(node.els.isDefined){
          sb.append("else")
          sb.append("{")
          getPrettyString(node.els.get,sb)
          sb.append("}")
        }
      case node: While =>
        sb.append("while (")
        getPrettyString(node.cond,sb)
        sb.append(")")
        getPrettyString(node.body,sb)
      case node: Println =>
        sb.append("println (")
        getPrettyString(node.expr,sb)
        sb.append(")")
      case node: Assign =>
        getPrettyString(node.id,sb)
        sb.append("=")
        getPrettyString(node.expr,sb)
      case _ => Reporter.fatal("Wring node type in tree",branch)
    }
  }
}
