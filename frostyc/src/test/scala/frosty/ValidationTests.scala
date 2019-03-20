package frosty

import org.scalatest._
import java.io.{File, FilenameFilter}
import scala.sys.process._

class ValidationTests extends FunSuite with Matchers {

  /** Files in the validation suite that should be used as tests should start
    * with "test", followed by a number, followed by underscore, followed by
    * a name consisting of `[A-Za-z0-9_]`, followed by a single dot, and then
    * by the file ending.
    */
  private val TestFileNameRegex = "test(\\d+)_([a-zA-Z0-9_]+)\\..*".r

  /** The expected output is saved in the comments in the test files themselves.
    * For now, only a single format (basic string) is supported.
    */
  private val ExpectRegex = "^[/* ]*EXPECT(?:ED)?:\\s*\"(.*)\"[ /*]*$".r

  /** Interprets `\n`, `\\` and `\"` in a string. */
  private def unescape(rawFromFile: String): String = {
    rawFromFile
      .replaceAll("\\\\\"", "\"")
      .replaceAll("\\\\\\\\", "\\\\")
      .replaceAll("\\\\n", "\n")
  }

  // Who tests the tester...?
  assert(unescape(""" \"Hi\" """) == " \"Hi\" ")
  assert(unescape(""" A\nB """) == " A\nB ")
  assert(unescape(""" A\\B """) ==  " A\\B ")

  /** Enumerates all test files in the specified / discovered validation 
    * suite, or returns `None` if no validation suite could be found.
    */
  private def discoverValidationSuite: Option[List[File]] = {
    Option(System.getenv("VALIDATION_SUITE"))
      .filterNot(_.isEmpty)
      .map(new File(_))
      .filter(_.exists)
      .orElse {
        Option(new File("../validation")).filter(_.exists)
      }
      .filter(_.isDirectory)
      .map{ 
        _.listFiles(new FilenameFilter {
          def accept(dir: File, name: String): Boolean = {
            TestFileNameRegex.pattern.matcher(name).matches
          }
        }).toList
      }
  }

  /** Tries to find the binary with the virtual machine. Looks in the 
    * environment variable first. If the file doesn't exist, attempts to 
    * find a sibling Rust project, build the virtual machine with Cargo,
    * and use the `debug` version.
    */
  lazy val discoverVirtualMachine: Option[File] = /* `lazy` is synchronized */ {
    Option(System.getenv("VALIDATION_VM"))
      .filterNot(_.isEmpty)
      .map(new File(_))
      .filter( f => {
        val ex = f.exists
        if (!ex) {
          System.err.println("Specified VM at " + f + " does not exist.")
        }
        ex
      })
      .filter(_.isFile)
      .orElse{
        System.err.println(
          "Trying to build VM in a sibling Rust project (that definitely " +
          "will fail on non-Linux/Unix OS'es)."
        )
        val parentDir = new File("../")
        if (parentDir.exists) {
          val probableRustVmProject = parentDir
            .listFiles
            .find{ f => 
              f.isDirectory && 
              f.getName.endsWith("vm") &&
              f.listFiles.map(_.getName).contains("Cargo.toml") // yay, Cargo!
            }
          probableRustVmProject.flatMap { rustBase => 
            // Don't need to check whether the executable already exists:
            // cargo is smart enough to figure it out, it's a build system!
            if (Process(Seq("cargo", "build"), rustBase).! == 0) {
              val targetDir = new File(
                rustBase.getAbsolutePath + "/target/debug/"
              )
              if (targetDir.exists) {
                targetDir
                  .listFiles(new FilenameFilter() {
                    def accept(dir: File, name: String) = name.endsWith("_vm")
                  })
                  .filter(f => f.isFile && f.canExecute)
                  .headOption
              } else {
                System.err.println(
                  "Built virtual machine using Cargo, but failed to find the " +
                  "resulting executable. You possibly wanted to specify an " +
                  "executable in the `VALIDATION_VM` environment variable, " +
                  "place a valid Rust/Cargo VM implementation in a sibling " + 
                  "directory, or modify the tests."
                )
                None
              }
            } else {
              System.err.println(
                "Attempted to use `cargo` to build VM in `" + rustBase + "`" +
                " but `cargo build` failed."
              )
              None
            }
          }
        } else {
          None
        }
      }
  }

  // Programmatically generate test suite from the files
  discoverValidationSuite match {
    case None => ignore("No validation suite discovered") {}
    case Some(testFiles) => {
      discoverVirtualMachine match {
        case None => ignore("No virtual machine found") {}
        case Some(vmFile) => {
          for (f <- testFiles.sortBy(_.getName)) {
            val src = scala.io.Source.fromFile(f)
            val lines = src.getLines.toList
            src.close()
          
            val expected = lines
              .collect {
                case ExpectRegex(escapedStringContent) => 
                  unescape(escapedStringContent)
              }
              .mkString("\n")
          
            if (expected.isEmpty) {
              ignore(f.getName + " (no `EXPECT:` found)") {}
            } else {
              test(f.getName) {
                val outputFile = File.createTempFile(f.getName, ".procs")
                Main.runCompiler(Array(
                  "--output",
                  outputFile.getAbsolutePath,
                  f.getAbsolutePath
                )) match {
                  case Left((errors, exitCode)) => withClue(
                    errors.mkString("\n") + "\nExit code = " + exitCode
                  ) {
                    fail
                  }
                  case Right(_okItCompiled) => {
                    println(
                      "Invoking vm at " + vmFile.getAbsolutePath + " with " +
                      "\n" + outputFile.getAbsolutePath
                    )
                    val res = Seq(
                      vmFile.getAbsolutePath,
                      outputFile.getAbsolutePath
                    ).!!
                    res.trim shouldEqual expected.trim
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
