package frosty

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.InvalidPathException
import scala.util.{Left, Right, Either}

sealed trait TraceMode
case object TraceToConsole extends TraceMode
case object TraceToHtml extends TraceMode

case class Config(
  verbose: Boolean,
  inputFiles: List[String],
  outputPath: Option[String],
  traceMode: TraceMode,
  htmlTraceFile: String,
  showPhases: Boolean,
  selectedPhases: Set[PhaseName],
) {

  private def deriveOutputFileName(fileName: String): String = {
    val pointSplits = fileName.split("\\.")
    if (pointSplits.size > 1) {
      // has ending. Replace the ending.
      pointSplits.init.mkString("", ".", ".procs")
    } else {
      // not separated by points. Just append an ending.
      fileName + ".procs"
    }
  }

  /** Proposes output path in same directory, with a name derived from input
    * name. The default strategy when there is no output file specified.
    */
  /* TODO: remove cruft
  private def deriveOutputFilePath(absoluteInputPath: Path)
  : Either[String, Path] = {

    val fileName = absoluteInputPath.getFileName.toString
    assert(!fileName.startsWith("/") && !fileName.endsWith("/"))

    val outputFileName = deriveOutputFileName(fileName)
    // getParent can be null
    Option(absoluteInputPath.getParent)
      .map(_.resolve(outputFileName))
      .fold(Left("Could not access parent directory of input file"))(Right(_))
  }
  */

  def proposeOutputFilePath: Either[String, Path] = {
    outputPath match {
      case Some(p) => {
        try {
          val absoluteOutputPath = Paths.get(p).toAbsolutePath
          val file = absoluteOutputPath.toFile
          if (file.isDirectory) {
            if (inputFiles.isEmpty) {
              Left("Could not derive output file name: no input files")
            } else {
              val inputFileName = Paths
                .get(inputFiles.head)
                .getFileName
                .toString

              Right(
                absoluteOutputPath
                  .resolve(deriveOutputFileName(inputFileName))
              )
            }
          } else {
            Option(absoluteOutputPath.getParent) match {
              case None => Left("Invalid parent directory")
              case Some(parentDirPath) => {
                val parentDir = parentDirPath.toFile
                if (parentDir.exists && parentDir.isDirectory) {
                  Right(absoluteOutputPath)
                } else {
                  Left("Invalid output parent directory: " + parentDir)
                }
              }
            }
          }
        } catch {
          case e: InvalidPathException => {
            Left(s"Invalid output path: `${p}`")
          }
        }
      }
      case None => {
        if (inputFiles.isEmpty) {
          Left("Could not derive output file name: no input files")
        } else {
          try {
            val fileName = Paths
              .get(inputFiles.head)
              .getFileName
              .toString

            val pwd = Paths.get("")
            Right(pwd.resolve(deriveOutputFileName(fileName)))
          } catch {
            case e: InvalidPathException => {
              Left(
                "Invalid path to input file `inputFiles.head` " + 
                "(Exception: " + e.getMessage + ")"
              )
            }
          }
        }
      }
    }
  }

}

object Config {

  def parseArgs(args: Array[String]): Option[Config] = {

    val parser = new scopt.OptionParser[Config]("frostyc") {
      
      head("frostyc", "0.1.0")

      opt[Unit]('v', "verbose")
        .text("verbose output (e.g. shows settings)")
        .action { (u, c) => c.copy(verbose = true) }
      
      opt[Unit]("print-html")
        .text("save result of `--print` as formatted HTML file")
        .action { (x, c) => c.copy(traceMode = TraceToHtml) }

      opt[Unit]("print-console")
        .text("dump result of `--print` to the console as plain text")
        .action { (x, c) => c.copy(traceMode = TraceToConsole) }

      opt[Seq[String]]('p', "print")
        .text("shows intermediate results of specified compiler phases")
        .valueName("ph1,ph2,...")
        .validate { ps =>
          if (ps.forall(
            p => p == "all" ||
            PhaseName.isValidShortcut(p)
          )) {
            Right(Unit)
          } else {
            Left(
              "Invalid phases: `" +
              ps
              .toSet
              .filterNot(p => p == "all" || PhaseName.isValidShortcut(p))
              .mkString(", ") +
              "`; Available phases: " + PhaseName.allShortcuts.mkString(",")
            )
          }
        }
        .action { (x, c) => 
          c.copy(selectedPhases =
            if (x contains "all") PhaseName.values.toSet
            else x.toSet.map(PhaseName.forShortcut)
          )
        }

      opt[Unit]("show-phases")
        .text("Lists compilation phases with brief descriptions")
        .action { (x, c) => c.copy(showPhases = true) }

      opt[String]('o', "output")
        .text("Specify the output path")
        .valueName("outputPath")
        .action { (x, c) => c.copy(outputPath = Some(x)) }

      help("help").text("prints this help")

      arg[String]("<inputFiles>...")
        .text("source code input files")
        .unbounded()
        .optional()
        .action { (x, c) => 
          c.copy(inputFiles = x :: c.inputFiles)
        }

    }

    val DefaultTraceFilePath = "/tmp/frosty-print-trace.html"
    val DefaultConfig = Config(
      verbose = false,
      inputFiles = Nil,
      outputPath = None,
      traceMode = TraceToConsole,
      htmlTraceFile = DefaultTraceFilePath,
      showPhases = false,
      selectedPhases = Set.empty
    )

    parser.parse(args, DefaultConfig)
  }

  def verboseDisplay(conf: Config): String = {
    classOf[Config]
    .getDeclaredFields
    .map{ f => 
      f.setAccessible(true)
      val res = (f.getName, f.get(conf))
      f.setAccessible(false)
      res
    }
    .map { case (k, v) => "  %-16s= %s".format(k, v) }
    .mkString("Config(\n", "\n", "\n)")
  }
}