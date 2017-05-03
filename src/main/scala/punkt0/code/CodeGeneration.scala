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

    /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
    def generateClassFile(sourceName: String, ct: ClassDecl, dir: String): Unit = {

      val parent = ct.parent match {
        case Some(id) => Some(id.value)
        case _ => None
      }

      val classFile = new ClassFile(ct.id.value, parent)
      classFile.setSourceFile(sourceName)

      createFields(classFile,ct)

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
        addConstantVal(ch,varDec.expr)
        varDec.expr match {
          case True() => ch << IStore(index)
          case False() => ch << IStore(index)
          case Null() => ch << AStore(index)
          case New(id) => ch << AStore(index)
          case IntLit(i) => ch << IStore(index)
          case StringLit(s) => ch << AStore(index)
          case _ => ???
        }
      }

      ch << RETURN

      ch.freeze
    }

    def createFields(classFile: ClassFile, ct: ClassDecl): Unit = {
      val constructor = classFile.addConstructor().codeHandler

      constructor << ALOAD_0 << InvokeSpecial("java/lang/Object", "<init>", "()V")

      ct.vars.foreach {
        v =>
          classFile.addField(p0ToCafeType(v.tpe.getType), v.id.value)

          constructor << ALOAD_0

          addConstantVal(constructor,v.expr)

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

    // Now do the main declaration
    // ...
  }

   def addConstantVal (ch : CodeHandler, expr : ExprTree) : Unit = {
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
