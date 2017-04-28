package punkt0
package analyzer

import Symbols._
import punkt0.ast.Trees._

object Types {

  trait Typed {
    private var _tpe: Type = TUntyped

    def setType(tpe: Type): this.type = { _tpe = tpe; this }
    def getType: Type = _tpe
  }

  sealed abstract class Type {
    def isSubTypeOf(tpe: Type): Boolean
  }

  case object TError extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = true
    override def toString = "[error]"
  }

  case object TUntyped extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = false
    override def toString = "[untyped]"
  }

  case object TInt extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TInt => true
      case _ => false
    }
    override def toString = "Int"
  }

  case object TBoolean extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TBoolean => true
      case _ => false
    }
    override def toString = "Boolean"
  }

  case object TString extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TString => true
      case _ => false
    }
    override def toString = "String"
  }

  case object TUnit extends Type {
    override def isSubTypeOf(tpe: Type): Boolean = tpe match {
      case TUnit => true
      case _ => false
    }
    override def toString = "Unit"
  }

  case class TAnyRef(classSymbol: ClassSymbol) extends Type {
    override def isSubTypeOf(tpe: Type): Boolean ={
      if (tpe.toString == "AnyRef" || tpe.toString == toString){
        true
      }else {
        classSymbol.parent match {
          case Some(c) => c.getType.isSubTypeOf(tpe)
          case None => false
        }
      }
    }
    override def toString = classSymbol.name
  }

  // special object to implement the fact that all objects are its subclasses
  val anyRef = TAnyRef(new ClassSymbol("AnyRef"))


  def typeTree2Type(typeTree: TypeTree,globalScope: GlobalScope): Type ={
    typeTree match {
      case _: BooleanType => TBoolean
      case _: IntType => TInt
      case _: StringType => TString
      case _: UnitType => TUnit
      case id: Identifier =>
        globalScope.lookupClass(id.value) match {
          case Some(c) => TAnyRef(c)
          case None => TError
        }
    }
  }
}
