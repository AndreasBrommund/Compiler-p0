package punkt0
package analyzer

import ast.Trees._

import Symbols._
import Types._

object TypeChecking extends Phase[Program, Program] {

  /** Typechecking does not produce a value, but has the side effect of
    * attaching types to trees and potentially outputting error messages. */
  def run(prog: Program)(ctx: Context): Program = {

    for(v <- prog.main.vars){
      tcExpr(v.expr,v.tpe.getType)
    }
    for(e <- prog.main.exprs){
      tcExpr(e)
    }
    for(c <- prog.classes){
      for(v <- c.vars){
        tcExpr(v.expr,v.tpe.getType)
      }
      for(m <- c.methods){
        for(v <- m.vars){
          tcExpr(v.expr,v.tpe.getType)
        }
        for(e <- m.exprs){
          tcExpr(e)
        }
        tcExpr(m.retExpr,m.retType.getType)
      }
    }



    def tcExpr(expr: ExprTree, expected: Type*): Type = {
      val tpe: Type = expr match {

        case p: Plus =>
          val ltype = tcExpr(p.lhs, TInt, TString)
          val rtype = tcExpr(p.rhs, TInt, TString)
          val expTyp = (ltype, rtype) match {
            case (TInt, TInt) => TInt
            case (TString, TInt) | (TInt, TString) | (TString, TString) => TString
            case _ => TError
          }
          expr.setType(expTyp)
          expTyp
        case m: Minus =>
          tcExpr(m.lhs, TInt)
          tcExpr(m.rhs, TInt)
          expr.setType(TInt)
          TInt
        case m: Times =>
          tcExpr(m.lhs, TInt)
          tcExpr(m.rhs, TInt)
          expr.setType(TInt)
          TInt
        case d: Div =>
          tcExpr(d.lhs, TInt)
          tcExpr(d.rhs, TInt)
          expr.setType(TInt)
          TInt
        case a: And =>
          tcExpr(a.lhs, TBoolean)
          tcExpr(a.rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case o: Or =>
          tcExpr(o.lhs, TBoolean)
          tcExpr(o.rhs, TBoolean)
          expr.setType(TBoolean)
          TBoolean
        case l: LessThan =>
          tcExpr(l.lhs, TInt)
          tcExpr(l.rhs, TInt)
          expr.setType(TBoolean)
          TBoolean
        case e: Equals =>
          val typeLeft = tcExpr(e.lhs)
          val typeRight = tcExpr(e.rhs)
          e.setType(TBoolean)


          if (typeLeft == TUnit || typeLeft.getClass != typeRight.getClass) {
            TError
          } else {
            TBoolean
          }

        case mExpr: MethodCall =>
          //Check what the obj is a class type and not a primitive type
          val classSymbol = tcExpr(mExpr.obj, anyRef).asInstanceOf[TAnyRef].classSymbol
          val methodSymbol = classSymbol.lookupMethod(mExpr.meth.value)
          //Check if the method exist in the current class or in any subclass
          //Also check the signature of the method
          methodSymbol match {
            case Some(method) =>
              if (method.argList.size != mExpr.args.size) {
                TError //Not the same amount of arguments
              } else {
                for (i <- method.argList.indices) {
                  tcExpr(mExpr.args(i), method.argList(i).getType)
                }

                mExpr.meth.setSymbol(method)


                mExpr.setType(method.getType)
                method.getType
              }
            case _ => TError //The method doesn't not exit
          }
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
        case t: This =>
          t.getType
        case n: Null =>
          n.getType
        case n: New =>
          n.getType
        case n: Not =>
          tcExpr(n.expr, TBoolean)
          n.setType(TBoolean)
          TBoolean
        case b: Block =>

          if (b.exprs.length > 1) { //Unesesery if size is 1
            for (e <- b.exprs.init) {
              tcExpr(e,TUnit)
            }
          }
          var typ : Type = TUnit
          if(b.exprs.length > 0) {
             typ = tcExpr(b.exprs.last)
          }
          b.setType(typ)
          typ

        case i: If =>
          tcExpr(i.expr, TBoolean)
          i.els match {
            //If no else branch exist, the return type must be TUnit
            case Some(elseNode) =>
              val typ = (tcExpr(i.thn), tcExpr(elseNode)) match {
                //If both branches have the type null the if expression should have TAnyRef as its typ
                //If any of the else or then branches have null as a type
                //then the other branch will decide the type of the if expression
                case (TNull, TNull) => TNull
                case (TNull, typElse) => typElse
                case (typThen, TNull) => typThen
                case (thenType, elseType) =>
                  if (thenType.getClass == elseType.getClass) {
                    thenType match {
                      case TInt | TBoolean | TString | TUnit => thenType
                      case any: TAnyRef =>
                        var it = any //Iterate over all else subtypes until it
                        //finds the lowest common subtype with "then"
                        while (!elseType.isSubTypeOf(it)) {
                          it.classSymbol.parent match {
                            case Some(parent) => it = parent.getType.asInstanceOf[TAnyRef]
                            case None => it = anyRef
                          }
                        }
                        it
                      case _ => TError
                    }
                  } else {
                    TError
                  }
              }
              i.setType(typ)
              typ
            case _ =>
              i.setType(TUnit)
              tcExpr(i.thn, TUnit)

          }
        case w: While =>
          tcExpr(w.cond,TBoolean)
          tcExpr(w.body,TUnit)
          w.setType(TUnit)
          TUnit
        case p:Println =>
          tcExpr(p.expr,TString,TInt,TBoolean)
          p.setType(TUnit)
          TUnit
        case a:Assign =>
          tcExpr(a.expr,a.id.getType)
          a.setType(TUnit)
          TUnit
      }


      if (tpe == TUntyped) {
        sys.error("Can't have TUntyped as type. At pos: " + expr.posString)
      }

      // Check result and return a valid type in case of error
      if (expected.isEmpty) {
        if (tpe == TError) {
          Reporter.error("Type error", expr)
          TUnit
        } else {
          tpe
        }
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
