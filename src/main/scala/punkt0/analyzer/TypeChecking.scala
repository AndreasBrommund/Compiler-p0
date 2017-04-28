package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
   * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {
    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {
        case p: Plus =>
          val ltype = tcExpr(p.lhs,TInt,TString)
          val rtype = tcExpr(p.rhs,TInt,TString)
          val expTyp = (ltype,rtype) match {
            case (TInt,TInt) => TInt
            case (TString,TInt) | (TInt,TString) | (TString,TString) => TString
            case _ => TError
          }
          expr.setType(expTyp)
          expTyp
        case m: Minus  =>
          tcExpr(m.lhs,TInt)
          tcExpr(m.rhs,TInt)
          expr.setType(TInt)
          TInt
        case m: Times =>
          tcExpr(m.lhs,TInt)
          tcExpr(m.rhs,TInt)
          expr.setType(TInt)
          TInt
        case d: Div =>
          tcExpr(d.lhs,TInt)
          tcExpr(d.rhs,TInt)
          expr.setType(TInt)
          TInt
        case a: And =>
          tcExpr(a.lhs,TBoolean)
          tcExpr(a.rhs,TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case o: Or =>
          tcExpr(o.lhs,TBoolean)
          tcExpr(o.rhs,TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case l: LessThan =>
          tcExpr(l.lhs,TInt)
          tcExpr(l.rhs,TInt)
          expr.setType(TBoolean)
          TBoolean
        case e: Equals =>
          tcExpr(e.lhs,TInt)
          tcExpr(e.rhs,TInt)
          expr.setType(TBoolean)
          TBoolean
        case _: IntLit =>
          expr.setType(TInt)
          TInt
        case _: StringLit =>
          expr.setType(TString)
          TString
        case _: True =>
          expr.setType(TBoolean)
          TBoolean
        case _: False =>
          expr.setType(TBoolean)
          TBoolean
        case i: Identifier =>
          i.getType
      }


      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        tpe
      } else if (!expected.exists(e => tpe.isSubTypeOf(e))) {
        Reporter.error("Type error: expected: " + expected.toList.mkString(" or ") + ", found: " + tpe, expr)
        expected.head
      } else {
        tpe
      }
    }

    prog
  }

}
