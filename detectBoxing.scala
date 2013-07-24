import util._
import java.nio.file._

object shelling {
  import sys.process._
  @volatile var shellPath = "/bin/bash"
  /**
   * Represents the result of a process.
   */
  trait Op {
    def stdout: Seq[String]
    def stdoutString = stdout.mkString("\n")
    def stderr: Seq[String]
    def stderrString = stderr.mkString("\n")
    def exitCode: Int
  }
  object Op {
    def apply(ec: Int, outputLines: Seq[String], errorLines: Seq[String]) = new Op {
      val exitCode = ec
      val stdout = outputLines
      val stderr = errorLines
    }
  }
  implicit class ShellExecutor(val sc: StringContext) extends AnyVal {
    def sh(args: Any*): Op = {
      var stdout = Vector.empty[String]
      var stderr = Vector.empty[String]
      val ec = Seq(shellPath, "-c", sc.s(args:_*))!(ProcessLogger(line => stdout :+= line, line => stderr :+= line))
      Op(ec, stdout, stderr)
    }
  }

  trait Undoable {
    def undo: Op
  }
  object Undoable {
    def apply(op: Op, inverseOp: => Op) = new Undoable {
      def undo = if (op.exitCode == 0) inverseOp else op
    }
  }

  def move(from: Path, to: Path) = Undoable(sh"mv '$from' '$to'", sh"mv '$to' '$from'")
  def find(args: String): Seq[Path] = sh"find $args".stdout map (l => path(l))
  def path(arg: String, args: String*) = Paths.get(arg, args:_*)

  implicit class PathOps(val p: Path) extends AnyVal {
    def exists: Boolean = Files exists p
    def isDir: Boolean = Files isDirectory p
    def isFile: Boolean = !isDir
    def isReadable: Boolean = Files isReadable p
    def isWritable: Boolean = Files isWritable p
    def isExecutable: Boolean = Files isExecutable p
    def isSymLink: Boolean = Files isSymbolicLink p
    def isSameAs(other: Path) = Files.isSameFile(p, other)
    def size: Long = Files size p
    def children(glob: String = null): Seq[Path] = {
      import collection.JavaConverters._
      val stream = if (glob == null) Files.newDirectoryStream(p) else Files.newDirectoryStream(p, glob)
      val res = try stream.asScala.to[IndexedSeq] finally stream.close()
      res
    }
    def traverseDf(): Stream[Path] = children().toStream.map {
      case f if f.isDir => f #:: f.traverseDf()
      case f => Stream(f)
    }.flatten
    def traverseBf(): Stream[Path] = {
      val css = children().toStream
      css #::: css.map {
        case f if f.isDir => f.traverseBf()
        case _ => Stream.empty
      }.flatten
    }
  }
}
import shelling._

val classes = find("-path '*/classes/*.class'")
val javapsByExitCode = classes.par.map(p => p->sh"javap -c -p $p | grep -i box").seq.groupBy(_._2.exitCode)

for {
  boxingClasses <- javapsByExitCode.get(0)
  c <- boxingClasses
} println(Console.YELLOW + c._1 + Console.RESET + "\n  " + c._2.stdout.mkString("\n  "))

