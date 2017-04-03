package punkt0

import java.io.File

import lexer._
import ast._
import analyzer._


object Main {
  def processOptions(args: Array[String]): Context = {
    var ctx = Context()

    def processOption(args: List[String]): Unit = args match {
      case "--help" :: args =>
        ctx = ctx.copy(doHelp = true)
        processOption(args)

      case "-d" :: out :: args =>
        ctx = ctx.copy(outDir = Some(new File(out)))
        processOption(args)

      case "--tokens" :: args =>
        ctx = ctx.copy(doTokens = true)
        processOption(args)

      case "--ast" :: args =>
        ctx = ctx.copy(doAST = true)
        processOption(args)

      case "--print" :: args =>
        ctx = ctx.copy(doPrintMain = true)
        processOption(args)

      case "--symid" :: args =>
        ctx = ctx.copy(doSymbolIds =  true)
        processOption(args)

      case f :: args =>
        ctx = ctx.copy(file = Some(new File(f)))
        processOption(args)

      case List() =>
    }

    processOption(args.toList)

    if (ctx.doHelp) {
      displayHelp()
      sys.exit(0)
    }

    ctx
  }

  def displayHelp(): Unit = {
    println("Usage: <punkt0c> [options] <file>")
    println("Options include:")
    println(" --help        displays this help")
    println(" --tokens      print tokens")
    println(" --ast         print abstract syntax tree")
    println(" --print       pritty-print")
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    if(ctx.file.isEmpty) {
      sys.error("No file")
    }else if(ctx.doTokens){
      val tokenIterator = Lexer.run(ctx.file.get)(ctx)
      while(tokenIterator.hasNext) {
        val token = tokenIterator.next
        printf("%s(%d:%d)\n",token,token.line,token.column)
      }
      Reporter.terminateIfErrors()
    } else if(ctx.doAST) {
      val phase = Lexer.andThen(Parser)
      val ast = phase.run(ctx.file.get)(ctx)
      println(ast)
    } else if(ctx.doPrintMain || ctx.doSymbolIds){
      val phase = Lexer.andThen(Parser).andThen(NameAnalysis)
      val ast = phase.run(ctx.file.get)(ctx)
      println(Printer.apply(ast,ctx.doSymbolIds))
    }
  }
}
