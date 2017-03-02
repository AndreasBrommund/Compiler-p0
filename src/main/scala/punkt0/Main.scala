package punkt0

import java.io.File

import lexer._
import ast._


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
    println(" -d <outdir>   generates class files in the specified directory")
  }

  def displayTokens(tokenIterator: Iterator[Token]) : Unit = {
    while(tokenIterator.hasNext) {
      val token = tokenIterator.next
      printf("%s(%d:%d)\n",token,token.line,token.column)
    }
  }

  def main(args: Array[String]): Unit = {
    val ctx = processOptions(args)

    val tokenIterator = ctx.file match {
      case Some(file) => Lexer.run(file)(ctx)
      case None => sys.error("No file")//TODO Handle error
    }


    if(ctx.doTokens)
      displayTokens(tokenIterator)
    else if(ctx.doAST)
      print(Parser.run(tokenIterator)(ctx))



    Reporter.terminateIfErrors()

  }

}
