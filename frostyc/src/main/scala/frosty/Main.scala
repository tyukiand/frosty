package frosty

import cats.effect.IO
import frosty.bytecode.{Serializer, DataWriter}
import scala.util.{Left, Either, Right}

object Main {

  /** Attempts to compile the input and save the bytecode to disk.
    *
    * In case of failure, returns list of error messages and an exit code.
    * Does not `exit` itself.
    */
  def runCompiler(args: Array[String]): Either[(List[String], Int), Unit] = {
    Config.parseArgs(args) match {
      case None => Left((List("Failed to parse command line options."), 1))
      case Some(conf) => {
      
        if (conf.verbose) {
          println(Config.verboseDisplay(conf))
        }

        // TODO: this should be more generally configurable,
        // not only the phases, but also the type of the pipeline itself,
        // and also the tracing interpreter (file? console?)
        val (tracerPipeline, tracingInterpreter) =
          if (conf.traceMode == TraceToHtml) {
            throw new NotImplementedError(
              "Tracing to HTML not implemented yet."
            )
          } else if (conf.traceMode == TraceToConsole) {
            val pipeline =
              prettyprint
              .console
              .ConsoleTracerPipeline
              .configure(conf.selectedPhases)
            val interpreter = ConsoleIoTracingInterpreter

            (pipeline, interpreter)
          } else {
            throw new AssertionError("Unknown trace mode.")
          }

        if (conf.showPhases) {
          for (PhaseDescription(name, descr) <- Compiler.description) {
            println(name + ":")
            println(descr.split("\n").map("  " + _).mkString("\n"))
          }
        }

        if (conf.inputFiles.isEmpty) {
          Left((List("No input files specified."), 2))
        } else {
          if (conf.inputFiles.forall(f => (new java.io.File(f)).exists)) {
            conf.proposeOutputFilePath match {
              case Left(err) => Left((List(err), 4))
              case Right(outputFilePath) => {
                val res = Compiler.run(
                  conf.inputFiles.map(SourceFile),
                  tracerPipeline,
                  tracingInterpreter
                ).unsafeRunSync
        
                res match {
                  case Left(errors) => {
                    Left((
                      errors.map(_.toString) :+ 
                      s"There were ${errors.size} errors.", 
                      100
                    ))
                  }
                  case Right(result) => {
                    Right((for {
                      w <- DataWriter.intoFile(outputFilePath.toFile)
                      _ <- Serializer.serialize(result)(w)
                    } yield ()).unsafeRunSync)
                  }
                }
              }
            }
          } else {
            Left((List(
              "Error: input files don't exist: " +
              conf
                .inputFiles
                .filterNot(f => (new java.io.File(f)).exists)
                .mkString
            ), 3))
          }
        }
      }
    }
  }


  def main(args: Array[String]): Unit = {
    runCompiler(args) match {
      case Left((errors, exitCode)) => {
        errors foreach println
        System.exit(exitCode)
      }
      case Right(_u) => /* good. */
    }
  }
}
