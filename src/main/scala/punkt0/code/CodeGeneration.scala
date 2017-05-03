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

      val parent = ct.parent match{
        case Some(id) => Some(id.value)
        case _ => None
      }

      val classFile = new ClassFile(ct.id.value,parent)
      classFile.setSourceFile(sourceName)

      ct.vars.foreach {
        c => classFile.addField(p0ToCafeType(c.tpe.getType),c.id.value)
      }


      ct.methods.foreach{
        m =>

          val argTypes = m.args.map{
            a => p0ToCafeType(a.tpe.getType)
          }

          val mh = classFile.addMethod(p0ToCafeType(m.retType.getType),m.id.value,argTypes)

          generateMethodCode(mh.codeHandler,m)
      }

      classFile.writeToFile(dir+ct.id.value+".class")

    }

    // a mapping from variable symbols to positions in the local variables
    // of the stack frame
    def generateMethodCode(ch: CodeHandler, mt: MethodDecl): Unit = {
      val methSym = mt.getSymbol

      // TODO: Emit code

      ch << IRETURN

      ch.freeze
    }

    val outDir = ctx.outDir.map(_.getPath+"/").getOrElse("./")

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

  private def p0ToCafeType(tpe : Type): String ={
    tpe match {
      case TBoolean => "Z"
      case TInt => "I"
      case TUnit => "V"
      case TString => "Ljava/lang/String;"
      case c : TAnyRef => "L"+c.toString+";"
      case t => sys.error("Illegal type in code generation: "+t)
    }
  }

}
