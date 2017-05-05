package punkt0
package code


import ast.Trees._
import analyzer.Symbols._
import analyzer.Types._
import cafebabe._
import AbstractByteCodes.{New => _, _}
import ByteCodes._


object CodeGeneration extends Phase[Program, Unit] {

  def run(prog: Program)(ctx: Context): Unit = {

    def generateClass(name : String, p: Option[Identifier],sourceName : String): ClassFile ={
      val parent = p match {
        case Some(id) => Some(id.value)
        case _ => None
      }

      val classFile = new ClassFile(name, parent)
      classFile.setSourceFile(sourceName)

      classFile
    }

    def generateMainFile(sourceName: String, mainDecl : MainDecl, dir: String): Unit ={
      //TODO maybe need to add parent
      val classFile = generateClass(mainDecl.obj.value,None,sourceName)

      var symbolPositions = Map[VariableSymbol,Int]()

      val ch = classFile.addMainMethod.codeHandler

      mainDecl.vars foreach {
        v =>
          val index = ch.getFreshVar(p0ToCafeType(v.tpe.getType))
          symbolPositions += (v.getSymbol -> index)
          assignConstant(ch,index,v.expr)
      }

      mainDecl.exprs foreach {
        e => buildJVMStack(ch,e,symbolPositions,mainDecl.getSymbol.name)
      }

      ch << RETURN

      ch.freeze
      classFile.writeToFile(dir + mainDecl.obj.value + ".class")
    }

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {


      val classFile = generateClass(ct.id.value,ct.parent,sourceName)

      createFields(classFile,ct.vars,ct.parent)

      ct.methods.foreach {
        m =>

          val argTypes = m.args.map {
            a => p0ToCafeType(a.tpe.getType)
          }

          val mh = classFile.addMethod(p0ToCafeType(m.retType.getType),m.id.value,argTypes)

          generateMethodCode(mh.codeHandler,m)
      }

      classFile.writeToFile(dir + ct.id.value + ".class")

    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      var symbolPositions = Map[VariableSymbol,Int]()

      for(i <- methSym.argList.indices){
        symbolPositions += (methSym.argList(i) -> (i+1))
      }

      for(varDec <- mt.vars){
        val index = ch.getFreshVar(p0ToCafeType(varDec.tpe.getType))
        symbolPositions += (varDec.getSymbol -> index)
        assignConstant(ch,index,varDec.expr)
      }

      mt.exprs foreach {
        e => buildJVMStack(ch,e,symbolPositions,methSym.classSymbol.name)
      }
      buildJVMStack(ch,mt.retExpr,symbolPositions,methSym.classSymbol.name)

      val returnInstr = mt.retType.getType match {
        case TInt | TBoolean => IRETURN
        case _ : TAnyRef | TString => ARETURN
        case TUnit => RETURN
        case _ => ???
      }
      ch << returnInstr

      ch.freeze
    }

    def createFields(classFile: ClassFile, vars: List[VarDecl],parent: Option[Identifier]): Unit = {

      val constructor = classFile.addConstructor().codeHandler

      val parentName = parent match {
        case Some(id) => id.value
        case None => "java/lang/Object"
      }

      constructor << ALOAD_0 << InvokeSpecial(parentName, "<init>", "()V")


      vars.foreach {
        v =>
          classFile.addField(p0ToCafeType(v.tpe.getType), v.id.value)

          constructor << ALOAD_0

          addConstantToStack(constructor,v.expr)

          constructor << PutField(classFile.className, v.id.value, p0ToCafeType(v.tpe.getType))
      }

      constructor << RETURN
      constructor.freeze
    }

    val outDir = ctx.outDir.map(_.getPath + "/").getOrElse("./")

    val f = new java.io.File(outDir)
    if (!f.exists()) {
      f.mkdir()
    }

    val sourceName = ctx.file.get.getName

    // output code
    prog.classes foreach {
      ct => generateClassFile(sourceName, ct, outDir)
    }

    generateMainFile(sourceName,prog.main,outDir)

  }

   def addConstantToStack (ch : CodeHandler, expr : ExprTree) : Unit = {
     expr match {
       case True() => ch << ICONST_1
       case False() => ch << ICONST_0
       case Null() => ch << ACONST_NULL
       case New(id) => ch << DefaultNew(id.value)
       case IntLit(i) => ch << Ldc(i)
       case StringLit(s) => ch << Ldc(s)
       case _ => ???
     }
  }

  def assignConstant(ch : CodeHandler, index : Int, expr : ExprTree) : Unit = {
    addConstantToStack(ch,expr)
    expr match {
      case True() => ch << IStore(index)
      case False() => ch << IStore(index)
      case Null() => ch << AStore(index)
      case New(id) => ch << AStore(index)
      case IntLit(i) => ch << IStore(index)
      case StringLit(s) => ch << AStore(index)
      case _ => ???
    }
  }

  def buildJVMStack(ch : CodeHandler, exprTree: ExprTree, symPos: Map[VariableSymbol,Int],className : String): Unit ={
    exprTree match {

      case And(lhs,rhs) =>
        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")

        buildJVMStack(ch,lhs,symPos,className)

        ch << IfEq(els)
        buildJVMStack(ch,rhs,symPos,className)
        ch << Goto(after) << Label(els) << ICONST_0 << Label(after)

      case Or(lhs,rhs) =>
        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")

        buildJVMStack(ch,lhs,symPos,className)
        ch  << IfEq(els) << ICONST_1 << Goto(after) << Label(els)
        buildJVMStack(ch,rhs,symPos,className)
        ch << Label(after)

      case Times(lhs,rhs) =>
        buildJVMStack(ch,lhs,symPos,className)
        buildJVMStack(ch,rhs,symPos,className)
        ch << IMUL
      case Div(lhs,rhs) =>
        buildJVMStack(ch,lhs,symPos,className)
        buildJVMStack(ch,rhs,symPos,className)
        ch << IDIV
      case Equals(lhs,rhs) =>

        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")

        buildJVMStack(ch,lhs,symPos,className)
        buildJVMStack(ch,rhs,symPos,className)

        lhs.getType match {
          case TInt | TBoolean => ch << If_ICmpNe(els)
          case _ : TAnyRef | TString => ch << If_ACmpNe(els)
          case _ => ???
        }
        ch << ICONST_1 << Goto(after) << Label(els) << ICONST_0 << Label(after)



      case LessThan(lhs,rhs) =>

        val after = ch.getFreshLabel("after")
        val gte = ch.getFreshLabel("gte")

        buildJVMStack(ch,lhs,symPos,className)
        buildJVMStack(ch,rhs,symPos,className)
        ch << If_ICmpGe(gte) << ICONST_1 << Goto(after) << Label(gte) << ICONST_0 << Label(after)



      case Plus(lhs,rhs) =>

        exprTree.getType match{
          case TInt =>
            buildJVMStack(ch,lhs,symPos,className)
            buildJVMStack(ch,rhs,symPos,className)
            ch << IADD
          case TString =>

            val typLhs = p0ToCafeType(lhs.getType)
            val typRhs = p0ToCafeType(rhs.getType)

            ch << DefaultNew("java/lang/StringBuilder")

            buildJVMStack(ch,lhs,symPos,className)
            ch << InvokeVirtual("java/lang/StringBuilder","append","("+typLhs+")Ljava/lang/StringBuilder;")

            buildJVMStack(ch,rhs,symPos,className)
            ch << InvokeVirtual("java/lang/StringBuilder","append","("+typRhs+")Ljava/lang/StringBuilder;")

            ch << InvokeVirtual("java/lang/StringBuilder","toString","()Ljava/lang/String;")
          case _ => ???
        }
      case Minus(lhs,rhs) =>
        buildJVMStack(ch,lhs,symPos,className)
        buildJVMStack(ch,rhs,symPos,className)
        ch << ISUB
      case IntLit(i) => ch << Ldc(i)
      case StringLit(s) => ch << Ldc(s);
      case True() => ch << ICONST_1
      case False() => ch << ICONST_0
      case id : Identifier =>
        val varSym = id.getSymbol.asInstanceOf[VariableSymbol]

        symPos.get(varSym) match {
          case Some(poolIndex) =>
            id.getType match {
              case TInt | TBoolean => ch << ILoad(poolIndex)
              case  _ : TAnyRef | TString  => ch << ALoad(poolIndex)
              case _ => ???
            }
          case None =>
            ch << ALOAD_0 << GetField(className,id.value,p0ToCafeType(id.getType))



        }
      case  New(id)=>
        ch << DefaultNew(id.value)

      case Println(exp) =>
        val typeSignature = exp.getType match {
          case TString => "(Ljava/lang/String;)V"
          case TInt => "(I)V"
          case TBoolean => "(Z)V"
          case _ => ???
        }

        ch << GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;")
        buildJVMStack(ch,exp,symPos,className)
        ch << InvokeVirtual("java/io/PrintStream", "println", typeSignature)
      case m : MethodCall =>
        buildJVMStack(ch,m.obj,symPos,className)
        var methSignature = "(";
        m.args foreach {
          a =>
            methSignature += p0ToCafeType(a.getType)
            buildJVMStack(ch, a, symPos,className)
        }
        methSignature += ")" + p0ToCafeType(m.getType)

        ch << InvokeVirtual(m.obj.getType.asInstanceOf[TAnyRef].toString,m.meth.value,methSignature)
      case Block(exprs) =>
        exprs foreach(e => buildJVMStack(ch,e,symPos,className))
      case This() => ch << ALOAD_0
      case Null() => ch << ACONST_NULL
      case Not(exp) =>
        val els = ch.getFreshLabel("else")
        val after = ch.getFreshLabel("after")

        buildJVMStack(ch,exp,symPos,className)

        ch << IfEq(els) << ICONST_0 << Goto(after) << Label(els) << ICONST_1 << Label(after)
      case If(exp,thn,els) =>
        val elseLabel = ch.getFreshLabel("else")
        val afterLabel = ch.getFreshLabel("after")

        buildJVMStack(ch,exp,symPos,className)
        ch << IfEq(elseLabel)
        buildJVMStack(ch,thn,symPos,className)
        ch << Goto(afterLabel)
        ch << Label(elseLabel)
        els match {
          case Some(elseExp) => buildJVMStack(ch,elseExp,symPos,className)
          case None =>
        }
        ch << Label(afterLabel)
      case Assign(id,exp) =>


        val varSym = id.getSymbol.asInstanceOf[VariableSymbol]

        symPos.get(varSym) match {
          case Some(poolIndex) =>
            buildJVMStack(ch,exp,symPos,className)
            id.getType match {
              case TInt | TBoolean => ch << IStore(poolIndex)
              case  _ : TAnyRef | TString  => ch << AStore(poolIndex)
              case _ => ???
            }
          case None =>
            ch << ALOAD_0
            buildJVMStack(ch,exp,symPos,className)
            ch << PutField(className,id.value,p0ToCafeType(id.getType))
        }
      case While(cond,body) =>
        val start = ch.getFreshLabel("start")
        val exit = ch.getFreshLabel("exit")
        ch << Label(start)
        buildJVMStack(ch,cond,symPos,className)
        ch << IfEq(exit)
        buildJVMStack(ch,body,symPos,className)
        ch << Goto(start) << Label(exit)
      case _ => ???
    }
  }

  private def p0ToCafeType(tpe: Type): String = {
    tpe match {
      case TBoolean => "Z"
      case TInt => "I"
      case TUnit => "V"
      case TString => "Ljava/lang/String;"
      case c: TAnyRef => "L" + c.toString + ";"
      case t => sys.error("Illegal type in code generation: " + t)
    }
  }

}
